/*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
   --------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>

#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <sqlite3.h>

#if SQLITE_VERSION_NUMBER < 3027002
#error "Unsupported SQLite version, at least 3.27.2 is needed"
#endif

#define Sqlite3_val(v) (*((sqlite3 **) Data_abstract_val(v)))
#define Sqlite3_stmt_val(v) (*((sqlite3_stmt **) Data_abstract_val(v)))
#define Sqlite3_backup_val(v) (*((sqlite3_backup **) Data_abstract_val(v)))
#define Sqlite3_rc_val(v) Int_val(v)
#define Val_sqlite3_rc(v) Val_int(v)

/* Library information and configuration */

CAMLprim value ocaml_rel_sqlite3_version_number (value unit)
{
  return (Val_int (sqlite3_libversion_number ()));
}

CAMLprim value ocaml_rel_sqlite3_errstr (value rc)
{
  return caml_copy_string (sqlite3_errstr (Sqlite3_rc_val (rc)));
}

/* Database connections */

CAMLprim value ocaml_rel_sqlite3_open (value file, value uri, value mode,
                                       value mutex, value vfs)
{
  CAMLparam5 (file, uri, mode, mutex, vfs);
  CAMLlocal2 (ret, db);

  if (!caml_string_is_c_safe (file))
    caml_invalid_argument ("sqlite3_open: file path string is not C safe.");

  if (!caml_string_is_c_safe (vfs))
    caml_invalid_argument ("sqlite3_open: vfs string is not C safe.");

  int flags = 0;
  switch (Int_val (mode)) {
  case 0: flags = SQLITE_OPEN_READONLY; break;
  case 1: flags = SQLITE_OPEN_READWRITE; break;
  case 2: flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE; break;
  case 3: flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_MEMORY; break;
  default: assert (false); break;
  }
  switch (Int_val (mutex)) {
  case 0: flags |= SQLITE_OPEN_NOMUTEX; break;
  case 1: flags |= SQLITE_OPEN_FULLMUTEX; break;
  default: assert (false); break;
  }
  if (Bool_val (uri)) { flags |= SQLITE_OPEN_URI; }

  sqlite3 *dbc = NULL;
  char *filec = caml_stat_strdup (String_val (file));
  char *vfsc =
    (caml_string_length (vfs) == 0) ? NULL :
    caml_stat_strdup (String_val (vfs));

  caml_release_runtime_system ();
  int rc = sqlite3_open_v2 (filec, &dbc, flags, vfsc);
  caml_stat_free (filec);
  if (vfsc != NULL) { caml_stat_free (vfsc); }
  caml_acquire_runtime_system ();

  if (rc == SQLITE_OK)
  {
    sqlite3_extended_result_codes (dbc, 1);
    value db = caml_alloc (1, Abstract_tag);
    *((sqlite3 **) Data_abstract_val(db)) = dbc;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, db);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_sqlite3_rc (rc));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_rel_sqlite3_close (value db)
{
  return Val_sqlite3_rc (sqlite3_close (Sqlite3_val (db)));
}

CAMLprim value ocaml_rel_sqlite3_errmsg (value db)
{
  return caml_copy_string (sqlite3_errmsg (Sqlite3_val (db)));
}

CAMLprim value ocaml_rel_sqlite3_extended_errcode (value db)
{
  return Val_int (sqlite3_extended_errcode (Sqlite3_val (db)));
}

CAMLprim value ocaml_rel_sqlite3_busy_timeout (value db, value ms)
{
  return Val_sqlite3_rc (sqlite3_busy_timeout (Sqlite3_val(db), Int_val(ms)));
}

CAMLprim value ocaml_rel_sqlite3_changes (value db)
{
  return Val_int (sqlite3_changes (Sqlite3_val (db)));
}

CAMLprim value ocaml_rel_sqlite3_last_insert_rowid (value db)
{
  return caml_copy_int64
    (sqlite3_last_insert_rowid (Sqlite3_val (db)));
}

/* Queries */

CAMLprim value ocaml_rel_sqlite3_exec (value db, value sql)
{
  if (!caml_string_is_c_safe (sql))
    caml_invalid_argument ("sqlite3_exec: SQL string is not C safe.");

  sqlite3 *dbc = Sqlite3_val (db);
  char *sqlc = caml_stat_strdup (String_val (sql));
  caml_release_runtime_system ();
  int rc = sqlite3_exec (dbc, sqlc, NULL, NULL, NULL);
  caml_stat_free (sqlc);
  caml_acquire_runtime_system();
  return Val_sqlite3_rc (rc);
}

/* Prepared statements */

CAMLprim value ocaml_rel_sqlite3_stmt_errmsg (value stmt)
{
  sqlite3_stmt *stmtc = Sqlite3_stmt_val (stmt);
  sqlite3 *dbc = sqlite3_db_handle (stmtc);
  const char *err = sqlite3_errmsg (dbc);
  if (!err) err = "";
  return caml_copy_string (err);
}

CAMLprim value ocaml_rel_sqlite3_prepare (value db, value sql)
{
  CAMLparam2 (db, sql);
  CAMLlocal1 (ret);

  if (!caml_string_is_c_safe (sql))
    caml_invalid_argument ("sqlite3_prepare: SQL string is not C safe.");

  sqlite3 *dbc = Sqlite3_val (db);
  sqlite3_stmt *stmtc = NULL;
  char *sqlc = caml_stat_strdup (String_val (sql));
  caml_release_runtime_system ();
  int rc = sqlite3_prepare_v2 (dbc, sqlc, strlen(sqlc), &stmtc, NULL);
  caml_stat_free (sqlc);
  caml_acquire_runtime_system();

  if (rc == SQLITE_OK)
  {
    value stmt = caml_alloc (1, Abstract_tag);
    *((sqlite3_stmt **) Data_abstract_val(stmt)) = stmtc;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, stmt);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_sqlite3_rc (rc));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_rel_sqlite3_finalize (value stmt)
{
  return Val_sqlite3_rc (sqlite3_finalize (Sqlite3_stmt_val (stmt)));
}

CAMLprim value ocaml_rel_sqlite3_reset (value stmt)
{
  return Val_sqlite3_rc (sqlite3_reset (Sqlite3_stmt_val (stmt)));
}

CAMLprim value ocaml_rel_sqlite3_step (value stmt)
{
  sqlite3_stmt *stmtc = Sqlite3_stmt_val (stmt);
  caml_release_runtime_system ();
  int rc = sqlite3_step (stmtc);
  caml_acquire_runtime_system ();
  return Val_sqlite3_rc (rc);
}

CAMLprim value ocaml_rel_sqlite3_column_count (value stmt)
{
  return Val_int (sqlite3_column_count (Sqlite3_stmt_val (stmt)));
}

CAMLprim value ocaml_rel_sqlite3_bind_paramater_count (value stmt)
{
  return Val_int (sqlite3_bind_parameter_count (Sqlite3_stmt_val (stmt)));
}

CAMLprim value ocaml_rel_sqlite3_bind_null (value stmt, value i)
{
  return Val_sqlite3_rc
    (sqlite3_bind_null (Sqlite3_stmt_val (stmt), Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_bind_bool (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_int64 (Sqlite3_stmt_val (stmt), Int_val (i),
                         Bool_val (v) ? 1 : 0));
}

CAMLprim value ocaml_rel_sqlite3_bind_int (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_int64 (Sqlite3_stmt_val (stmt), Int_val (i), Long_val (v)));
}

CAMLprim value ocaml_rel_sqlite3_bind_int64 (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_int64 (Sqlite3_stmt_val (stmt), Int_val (i), Int64_val (v)));
}

CAMLprim value ocaml_rel_sqlite3_bind_double (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_double (Sqlite3_stmt_val (stmt), Int_val (i),
                          Double_val (v)));
}

CAMLprim value ocaml_rel_sqlite3_bind_text (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_text (Sqlite3_stmt_val (stmt), Int_val (i),
                        String_val (v), caml_string_length (v),
                        SQLITE_TRANSIENT));
}

CAMLprim value ocaml_rel_sqlite3_bind_blob (value stmt, value i, value v)
{
  return Val_sqlite3_rc
    (sqlite3_bind_blob (Sqlite3_stmt_val (stmt), Int_val (i),
                        String_val (v), caml_string_length (v),
                        SQLITE_TRANSIENT));
}

CAMLprim value ocaml_rel_sqlite3_clear_bindings (value stmt)
{
  return Val_sqlite3_rc (sqlite3_clear_bindings (Sqlite3_stmt_val (stmt)));
}

CAMLprim value ocaml_rel_sqlite3_column_is_null (value stmt, value i)
{
  return Val_bool (sqlite3_column_type (Sqlite3_stmt_val (stmt), Int_val (i))
                   == SQLITE_NULL);
}

CAMLprim value ocaml_rel_sqlite3_column_bool (value stmt, value i)
{
  return Val_bool (sqlite3_column_int (Sqlite3_stmt_val (stmt), Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_column_int (value stmt, value i)
{
  return Val_int (sqlite3_column_int64 (Sqlite3_stmt_val (stmt), Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_column_int64 (value stmt, value i)
{
  return caml_copy_int64
    (sqlite3_column_int64 (Sqlite3_stmt_val (stmt), Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_column_double (value stmt, value i)
{
  return caml_copy_double
    (sqlite3_column_double (Sqlite3_stmt_val (stmt), Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_column_text (value stmt, value i)
{
  sqlite3_stmt *stmtc = Sqlite3_stmt_val (stmt);
  int len = sqlite3_column_bytes (stmtc, Int_val (i));
  return caml_alloc_initialized_string
    (len, (char *)sqlite3_column_text (stmtc, Int_val (i)));
}

CAMLprim value ocaml_rel_sqlite3_column_blob (value stmt, value i)
{
  sqlite3_stmt *stmtc = Sqlite3_stmt_val (stmt);
  int len = sqlite3_column_bytes (stmtc, Int_val (i));
  return caml_alloc_initialized_string
    (len, (char *)sqlite3_column_blob (stmtc, Int_val (i)));
}

/* Backups */

CAMLprim value ocaml_rel_sqlite3_backup_init
(value dst, value dname, value src, value sname)
{
  CAMLparam4 (dst, dname, src, sname);
  CAMLlocal2 (ret, b);

  if (!caml_string_is_c_safe (dname))
    caml_invalid_argument
      ("sqlite3_backup_init: destination database name is not C safe.");

  if (!caml_string_is_c_safe (sname))
    caml_invalid_argument
      ("sqlite3_backup_init: source database name is not C safe.");

  sqlite3_backup *bc = NULL;
  sqlite3 *dstc = Sqlite3_val (dst);
  sqlite3 *srcc = Sqlite3_val (src);
  char *dnamec = caml_stat_strdup (String_val (dname));
  char *snamec = caml_stat_strdup (String_val (sname));
  caml_release_runtime_system ();
  bc = sqlite3_backup_init (dstc, dnamec, srcc, snamec);
  caml_stat_free (dnamec);
  caml_stat_free (snamec);
  caml_acquire_runtime_system ();

  if (bc)
  {
    value b = caml_alloc (1, Abstract_tag);
    *((sqlite3_backup **) Data_abstract_val (b)) = bc;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, b);
  } else {
    int rc = sqlite3_errcode (dstc);
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_sqlite3_rc (rc));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_rel_sqlite3_backup_finish (value b)
{
  return Val_sqlite3_rc (sqlite3_backup_finish (Sqlite3_backup_val (b)));
}

CAMLprim value ocaml_rel_sqlite3_backup_step (value b, value n)
{

  sqlite3_backup *bc = Sqlite3_backup_val (b);
  int nc = Int_val (n);
  caml_release_runtime_system ();
  int rc = sqlite3_backup_step (bc, nc);
  caml_acquire_runtime_system();
  return Val_sqlite3_rc (rc);
}

CAMLprim value ocaml_rel_sqlite3_backup_remaining (value b)
{
  return Val_int (sqlite3_backup_remaining (Sqlite3_backup_val (b)));
}

CAMLprim value ocaml_rel_sqlite3_backup_pagecount (value b)
{
  return Val_int (sqlite3_backup_pagecount (Sqlite3_backup_val (b)));
}
