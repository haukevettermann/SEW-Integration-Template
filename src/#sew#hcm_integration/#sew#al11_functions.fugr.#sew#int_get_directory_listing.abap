FUNCTION /SEW/INT_GET_DIRECTORY_LISTING.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(DIR_NAME) LIKE  EPSF-EPSDIRNAM
*"     VALUE(FILE_MASK) LIKE  EPSF-EPSFILNAM DEFAULT SPACE
*"  EXPORTING
*"     VALUE(DIR_NAME) LIKE  EPSF-EPSDIRNAM
*"     VALUE(FILE_COUNTER) LIKE  EPSF-EPSFILSIZ
*"     VALUE(ERROR_COUNTER) LIKE  EPSF-EPSFILSIZ
*"  TABLES
*"      DIR_LIST STRUCTURE  /SEW/FILE_LIST
*"  EXCEPTIONS
*"      INVALID_EPS_SUBDIR
*"      SAPGPARAM_FAILED
*"      BUILD_DIRECTORY_FAILED
*"      NO_AUTHORIZATION
*"      READ_DIRECTORY_FAILED
*"      TOO_MANY_READ_ERRORS
*"      EMPTY_DIRECTORY_LIST
*"----------------------------------------------------------------------

DATA: BEGIN OF file,
        dirname(75) TYPE c, " name of directory. (possibly truncated.)
        name(200)    TYPE c, " name of entry. (possibly truncated.)
        type(10)    TYPE c, " type of entry.
        len(8)      TYPE p, " length in bytes.
        owner(8)    TYPE c, " owner of the entry.
        mtime(6)    TYPE p, " last modification date, seconds since 1970
        mode(9)     TYPE c, " like "rwx-r-x--x": protection mode.
        errno(3)    TYPE c,
        errmsg(40)  TYPE c,
      END OF file.

DATA: lv_eps_subdir     LIKE epsf-epssubdir.

* authority check
  IF dir_name(4) = '$TR_'.   " check TMS authority
*    PERFORM check_trans_read_authority.
*    IF sy-subrc <> 0.
*      RAISE no_authorization.
*    ENDIF.
  ELSE.
*    PERFORM check_ftp_authority.
*    IF sy-subrc <> 0.
*      RAISE no_authorization.
*    ENDIF.
  ENDIF.

* expand EPS subdirectory names
  IF dir_name = space.                      " assume files in EPS/in
    dir_name = 'in'.
  ENDIF.
  IF dir_name = 'in'  OR               " get full directory name
     dir_name = 'out' OR
     dir_name = 'log' OR
     dir_name(4) = '$TR_'.
    lv_eps_subdir = dir_name.
    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
         EXPORTING
             eps_subdir             = lv_eps_subdir
             dir_name               = dir_name
         IMPORTING
             dir_name               = dir_name
         EXCEPTIONS
             invalid_eps_subdir     = 01
             sapgparam_failed       = 02
             build_directory_failed = 03.
    CASE sy-subrc.
      WHEN 01.
        RAISE invalid_eps_subdir.
      WHEN 02.
        RAISE sapgparam_failed.
      WHEN 03.
        RAISE build_directory_failed.
    ENDCASE.
  ENDIF.

* get directory listing
  CALL 'C_DIR_READ_FINISH'                  " just to be sure
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.

  CALL 'C_DIR_READ_START'
        ID 'DIR'    FIELD dir_name
        ID 'FILE'   FIELD file_mask
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.
  IF sy-subrc <> 0.
    RAISE read_directory_failed.
  ENDIF.

  REFRESH dir_list.
  CLEAR file_counter.
  CLEAR error_counter.
  DO.
    CLEAR file.
    CLEAR dir_list.
    CALL 'C_DIR_READ_NEXT'
          ID 'TYPE'   FIELD file-type
          ID 'NAME'   FIELD file-name
          ID 'LEN'    FIELD file-len
          ID 'OWNER'  FIELD file-owner
          ID 'MTIME'  FIELD file-mtime
          ID 'MODE'   FIELD file-mode
          ID 'ERRNO'  FIELD file-errno
          ID 'ERRMSG' FIELD file-errmsg.

*   handle files > 2147483647 byte (int 4) - hen 9.9.2004
    IF file-len > 2147483647.
      dir_list-size  = -99.
    ELSE.
      dir_list-size  = file-len.
    ENDIF.
    dir_list-name = file-name.
    IF sy-subrc = 0.
      IF file-type(1) = 'f' OR              " regular file
         file-type(1) = 'F'.
        ADD 1 TO file_counter.
        dir_list-rc   = 0.
        APPEND dir_list.
      ENDIF.
    ELSEIF sy-subrc = 1.
      EXIT.
    ELSE.
      IF error_counter > 1000.
        CALL 'C_DIR_READ_FINISH'
              ID 'ERRNO'  FIELD file-errno
              ID 'ERRMSG' FIELD file-errmsg.
        RAISE too_many_read_errors.
      ENDIF.
      ADD 1 TO error_counter.
      dir_list-rc  = 18.
      APPEND dir_list.
    ENDIF.
  ENDDO.

  CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.

  IF file_counter > 0.
    SORT dir_list BY name ASCENDING.
  ELSE.
    RAISE empty_directory_list.
  ENDIF.

ENDFUNCTION.
