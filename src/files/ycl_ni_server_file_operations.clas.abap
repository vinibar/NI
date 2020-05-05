"! Helper class for file server manipulation
"! Developed by @vinibar at April 2020 (covid-19 lockdown)
"! License: MIT https://raw.githubusercontent.com/vinibar/ni/master/LICENSE
"! Copyright (c) 2020 ni Contributors
CLASS ycl_ni_server_file_operations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_error_number TYPE c LENGTH 9,
      ty_error_msg    TYPE c LENGTH 40,
      ty_dirname      TYPE c LENGTH 100.

    TYPES:
      BEGIN OF ty_file_list,
        dirname      TYPE c LENGTH 100,
        filename     TYPE c LENGTH 100,
        type         TYPE c LENGTH 10,
        len          TYPE p LENGTH 8 DECIMALS 2,
        owner        TYPE c LENGTH 20,
        mtime        TYPE p LENGTH 6 DECIMALS 2,
        mode         TYPE c LENGTH 9,
        error_number TYPE ty_error_number,
        error_msg    TYPE ty_error_msg,
        full_path    TYPE string,
      END OF ty_file_list.
    TYPES: tt_file_list TYPE TABLE OF ty_file_list WITH KEY filename.
    TYPES: tt_file_content_txt TYPE TABLE OF string WITH KEY table_line.
    TYPES: tt_file_content_bin TYPE TABLE OF xstring WITH KEY table_line.

    METHODS constructor
      IMPORTING iv_authority_check TYPE sap_bool DEFAULT abap_false.

    "! Open text file content
    "! @parameter iv_full_path | Full filename with path
    "! @parameter rt_content | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS read_txt_file
      IMPORTING iv_full_path      TYPE string
      RETURNING VALUE(rt_content) TYPE tt_file_content_txt
      RAISING   ycx_ni_file_operations.

    "! Open binary file content
    "! @parameter iv_full_path | Full filename with path
    "! @parameter rt_content | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS read_bin_file
      IMPORTING iv_full_path      TYPE string
      RETURNING VALUE(rt_content) TYPE tt_file_content_bin
      RAISING   ycx_ni_file_operations.

    "! Write text content to file
    "! @parameter iv_full_path | Full filename with path
    "! @parameter it_content | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS write_txt_file
      IMPORTING
                iv_full_path TYPE string
                it_content   TYPE tt_file_content_txt
                iv_overwrite TYPE sap_bool OPTIONAL
      RAISING   ycx_ni_file_operations.

    "! Write binary content to file
    "! @parameter iv_full_path | Full filename with path
    "! @parameter it_content | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS write_bin_file
      IMPORTING
                iv_full_path TYPE string
                it_content   TYPE tt_file_content_bin
                iv_overwrite TYPE sap_bool OPTIONAL
      RAISING   ycx_ni_file_operations.

    "! List all files in a given directory path
    "! @parameter iv_dirname | Full filename with path
    "! @parameter rt_files | List of filenames
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS list_files
      IMPORTING
                iv_dirname      TYPE ty_dirname
                iv_file_mask    TYPE string DEFAULT '*'
      RETURNING VALUE(rt_files) TYPE tt_file_list
      RAISING   ycx_ni_file_operations.

    "! Copy file
    "! @parameter iv_source_full_path | Source full filename with path
    "! @parameter iv_dest_full_path | Destination full filename with path
    "! @parameter iv_overwrite | Overwrite destination file? 'X' = Yes, '' = No
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS copy_file
      IMPORTING
                iv_source_full_path TYPE string
                iv_dest_full_path   TYPE string
                iv_overwrite        TYPE sap_bool OPTIONAL
      RAISING   ycx_ni_file_operations.

    "! Delete file
    "! @parameter iv_full_path | Full filename with path to be deleted
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS delete_file
      IMPORTING iv_full_path TYPE string
      RAISING   ycx_ni_file_operations.

    "! Move file
    "! @parameter iv_source_full_path | Source full filename with path
    "! @parameter iv_dest_full_path | Destination full filename with path
    "! @parameter iv_overwrite | Overwrite destination file? 'X' = Yes, '' = No
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS move_file
      IMPORTING
                iv_source_full_path TYPE string
                iv_dest_full_path   TYPE string
                iv_overwrite        TYPE sap_bool OPTIONAL
      RAISING   ycx_ni_file_operations.

  PROTECTED SECTION.

    DATA mv_authority_check TYPE sap_bool.

    "! Handle standard messages returned by sy-subrc on function calls
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS raise_comm_error_from_sy
      RAISING ycx_ni_file_operations.

    "! Split full path into directory path and filename
    "! @parameter iv_full_path | Full path
    "! @parameter ev_dirname | Directory path
    "! @parameter ev_filename | Filename
    METHODS split_full_path
      IMPORTING iv_full_path TYPE string
      EXPORTING
                ev_dirname   TYPE ty_dirname
                ev_filename  TYPE string.


    "! Check if the path of directory is valid
    "! @parameter iv_dirname | Directory path
    "! @parameter rv_valid | Is valid?
    METHODS directory_exists
      IMPORTING iv_dirname      TYPE ty_dirname
      RETURNING VALUE(rv_valid) TYPE sap_bool.

    "! Check if the file exists
    "! @parameter iv_full_path | Full path of file
    "! @parameter rv_valid | Is valid?
    METHODS file_exists
      IMPORTING iv_full_path    TYPE string
      RETURNING VALUE(rv_valid) TYPE sap_bool.

    "! Create the given directory
    "! @parameter iv_dirname | Directory path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS create_directory
      IMPORTING iv_dirname TYPE ty_dirname
      RAISING   ycx_ni_file_operations.

  PRIVATE SECTION.

    "! Create the given directory !!! INTERNAL USE ONLY. NEVER USE IR. I MEAN IT.
    "! @parameter iv_dirname | Directory path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS delete_directory
      IMPORTING
                iv_recursively TYPE sap_bool OPTIONAL
                iv_dirname     TYPE ty_dirname
      RAISING   ycx_ni_file_operations.

    METHODS authority_check_c_function
      RAISING ycx_ni_file_operations.

ENDCLASS.



CLASS ycl_ni_server_file_operations IMPLEMENTATION.



  METHOD constructor.
    mv_authority_check = iv_authority_check.
  ENDMETHOD.



  METHOD copy_file.

    DATA lt_file_contents TYPE tt_file_content_bin.
    lt_file_contents = read_bin_file( iv_source_full_path ).
    write_bin_file(
      iv_full_path = iv_dest_full_path
      it_content = lt_file_contents
      iv_overwrite = iv_overwrite ).

  ENDMETHOD.



  METHOD create_directory.

    DATA lv_command TYPE char255.
    DATA lt_result TYPE TABLE OF char255.

    IF mv_authority_check = abap_true.
      me->authority_check_c_function(  ).
    ENDIF.

    CONCATENATE 'mkdir' iv_dirname INTO lv_command
        SEPARATED BY space.

    CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command
                  ID 'TAB'     FIELD lt_result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Directory not created'.
    ENDIF.

  ENDMETHOD.



  METHOD delete_directory.

    DATA lv_command TYPE char255.
    DATA lt_result TYPE TABLE OF char255.

    IF mv_authority_check = abap_true.
      me->authority_check_c_function(  ).
    ENDIF.

    IF iv_recursively = abap_true.
      CONCATENATE 'rm -r' iv_dirname INTO lv_command
          SEPARATED BY space.
    ELSE.
      CONCATENATE 'rm -d' iv_dirname INTO lv_command
          SEPARATED BY space.
    ENDIF.

    CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command
                  ID 'TAB'     FIELD lt_result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Directory not deleted'.
    ENDIF.

  ENDMETHOD.



  METHOD authority_check_c_function.

    DATA lv_program TYPE authb-program.

    CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD lv_program.

    AUTHORITY-CHECK OBJECT 'S_C_FUNCT'
      ID 'PROGRAM'   FIELD lv_program
      ID 'ACTVT'     FIELD 'CALL'
      ID 'CFUNCNAME' FIELD 'SYSTEM'.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Permission denied'.
    ENDIF.

  ENDMETHOD.



  METHOD delete_file.

    DATA lv_filename TYPE epsfilnam.
    DATA lv_dirname TYPE epsdirnam.
    DATA ls_find_results TYPE match_result.

    IF file_exists( iv_full_path ) <> abap_true.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'File doesnt exist'.
    ENDIF.

    DELETE DATASET iv_full_path.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Permission denied'.
    ENDIF.

  ENDMETHOD.



  METHOD directory_exists.

    DATA lv_dirname TYPE btch0000-text80.
    DATA: lv_error_number TYPE ty_error_number,
          lv_error_msg    TYPE ty_error_msg.

    lv_dirname = iv_dirname.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD lv_error_number
        ID 'ERRMSG' FIELD lv_error_msg.

    CALL 'C_DIR_READ_START'
        ID 'DIR'    FIELD lv_dirname
        ID 'FILE'   FIELD '*'
        ID 'ERRNO'  FIELD lv_error_number
        ID 'ERRMSG' FIELD lv_error_msg.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

    CALL 'C_DIR_READ_NEXT'.
    CALL 'C_DIR_READ_FINISH'.

  ENDMETHOD.



  METHOD file_exists.

    DATA: lv_dirname  TYPE ty_dirname,
          lv_filename TYPE string.

    DATA: lv_error_number TYPE ty_error_number,
          lv_error_msg    TYPE ty_error_msg.

    DATA lt_files TYPE tt_file_list.

    split_full_path(
        EXPORTING iv_full_path = iv_full_path
        IMPORTING
            ev_dirname = lv_dirname
            ev_filename = lv_filename ).

    TRY.
        lt_files = list_files( iv_dirname  = lv_dirname iv_file_mask = lv_filename ).
      CATCH ycx_ni_file_operations.
        "nothing to do
    ENDTRY.

    READ TABLE lt_files TRANSPORTING NO FIELDS
        WITH KEY filename = lv_filename.
    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.



  METHOD list_files.

    DATA: lv_error_number TYPE ty_error_number,
          lv_error_msg    TYPE ty_error_msg.

    DATA ls_file LIKE LINE OF rt_files.
    DATA lv_dirname LIKE iv_dirname.

    IF directory_exists( iv_dirname ) <> abap_true.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Invalid path'.
    ENDIF.

    CALL 'C_DIR_READ_FINISH'.
    CALL 'C_DIR_READ_START'
        ID 'DIR'    FIELD iv_dirname
        ID 'FILE'   FIELD iv_file_mask
        ID 'ERRNO'  FIELD lv_error_number
        ID 'ERRMSG' FIELD lv_error_msg.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Cant list files'.
    ENDIF.

    DO.

      ls_file-dirname = iv_dirname.
      CALL 'C_DIR_READ_NEXT'
            ID 'TYPE'   FIELD ls_file-type
            ID 'NAME'   FIELD ls_file-filename
            ID 'LEN'    FIELD ls_file-len
            ID 'OWNER'  FIELD ls_file-owner
            ID 'MTIME'  FIELD ls_file-mtime
            ID 'MODE'   FIELD ls_file-mode
            ID 'ERRNO'  FIELD ls_file-error_number
            ID 'ERRMSG' FIELD ls_file-error_msg.

      IF sy-subrc = 1.
        CALL 'C_DIR_READ_FINISH'.
        EXIT.
      ENDIF.

      " ensures that there's a slash at the end of dirname
      lv_dirname = iv_dirname.
      FIND REGEX '\/$' IN iv_dirname.
      IF sy-subrc = 0.
        CONCATENATE lv_dirname '/' INTO lv_dirname.
      ENDIF.

      " Generate fullpath from dirname and filename
      CONCATENATE lv_dirname ls_file-filename INTO ls_file-full_path.

      APPEND ls_file TO rt_files.
      CLEAR ls_file.

    ENDDO.

    CALL 'C_DIR_READ_FINISH'
          ID 'ERRNO'  FIELD lv_error_number
          ID 'ERRMSG' FIELD lv_error_msg.

    SORT rt_files BY filename.
    DELETE rt_files WHERE type NS 'file'.

  ENDMETHOD.



  METHOD move_file.

    copy_file(
      iv_source_full_path = iv_source_full_path
      iv_dest_full_path = iv_dest_full_path
      iv_overwrite = iv_overwrite  ).

    delete_file( iv_source_full_path ).

  ENDMETHOD.



  METHOD raise_comm_error_from_sy.

    DATA lv_error_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_error_msg.

    CONCATENATE 'Operation error:' lv_error_msg INTO lv_error_msg SEPARATED BY space.

    RAISE EXCEPTION TYPE ycx_ni_file_operations
      EXPORTING
        iv_text = lv_error_msg.

  ENDMETHOD.



  METHOD read_txt_file.

    DATA ls_content LIKE LINE OF rt_content.
    DATA lv_msg TYPE string.

    DATA: lx_file_access_error TYPE REF TO cx_sy_file_access_error,
          lx_file_close        TYPE REF TO cx_sy_file_close.

    OPEN DATASET iv_full_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CONCATENATE 'Cant open' iv_full_path INTO lv_msg
          SEPARATED BY space.

      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = lv_msg.
    ENDIF.

    TRY.
        DO.
          READ DATASET iv_full_path INTO ls_content.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          APPEND ls_content TO rt_content.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE ycx_ni_file_operations
              EXPORTING
                iv_text = 'Cant insert line into content table'.
          ENDIF.
        ENDDO.
      CATCH cx_sy_file_access_error INTO lx_file_access_error.

        lv_msg = lx_file_access_error->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

    TRY.
        CLOSE DATASET iv_full_path.
      CATCH cx_sy_file_close INTO lx_file_close.
        lv_msg = lx_file_close->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

  ENDMETHOD.



  METHOD read_bin_file.

    DATA ls_content LIKE LINE OF rt_content.
    DATA lv_msg TYPE string.
    DATA lv_length TYPE i VALUE 1.

    DATA: lx_file_access_error TYPE REF TO cx_sy_file_access_error,
          lx_file_close        TYPE REF TO cx_sy_file_close.

    OPEN DATASET iv_full_path FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      CONCATENATE 'Cant open' iv_full_path INTO lv_msg
          SEPARATED BY space.

      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = lv_msg.
    ENDIF.

    TRY.
        WHILE lv_length > 0.
          READ DATASET iv_full_path INTO ls_content LENGTH lv_length.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          APPEND ls_content TO rt_content.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE ycx_ni_file_operations
              EXPORTING
                iv_text = 'Cant insert line into content table'.
          ENDIF.
        ENDWHILE.
      CATCH cx_sy_file_access_error INTO lx_file_access_error.

        lv_msg = lx_file_access_error->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

    TRY.
        CLOSE DATASET iv_full_path.
      CATCH cx_sy_file_close INTO lx_file_close.
        lv_msg = lx_file_close->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

  ENDMETHOD.



  METHOD split_full_path.

    DATA ls_find_results TYPE match_result.
    FIND REGEX '[^\\/:*?"<>|\r\n]+$' IN iv_full_path RESULTS ls_find_results.
    ev_filename = iv_full_path+ls_find_results-offset(ls_find_results-length).
    ev_dirname = iv_full_path.
    REPLACE REGEX '[^\\/:*?"<>|\r\n]+$' IN ev_dirname WITH space.

  ENDMETHOD.



  METHOD write_txt_file.

    DATA lv_dirname TYPE ty_dirname.
    DATA lv_msg TYPE string.

    DATA ls_content LIKE LINE OF it_content.

    DATA: lx_file_access_error TYPE REF TO cx_sy_file_access_error,
          lx_file_close        TYPE REF TO cx_sy_file_close.

    IF iv_overwrite = abap_true.
      split_full_path(
        EXPORTING
          iv_full_path = iv_full_path
        IMPORTING
          ev_dirname   = lv_dirname ).

      IF directory_exists( lv_dirname ) <> abap_true.
        create_directory( lv_dirname ).
      ENDIF.
    ENDIF.

    IF iv_overwrite = abap_true.
      OPEN DATASET iv_full_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8 WITH BYTE-ORDER MARK.
    ELSE.
      OPEN DATASET iv_full_path FOR APPENDING IN TEXT MODE ENCODING DEFAULT.
    ENDIF.

    IF sy-subrc <> 0.
      CONCATENATE 'Cant open' iv_full_path INTO lv_msg
          SEPARATED BY space.

      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = lv_msg.
    ENDIF.

    TRY.
        LOOP AT it_content INTO ls_content.
          TRANSFER ls_content TO iv_full_path.
        ENDLOOP.

      CATCH cx_sy_file_access_error INTO lx_file_access_error.
        lv_msg = lx_file_access_error->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

    TRY.
        CLOSE DATASET iv_full_path.
      CATCH cx_sy_file_close INTO lx_file_close.
        lv_msg = lx_file_close->get_text( ).

        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

  ENDMETHOD.



  METHOD write_bin_file.

    DATA lv_dirname TYPE ty_dirname.
    DATA lv_msg TYPE string.
    DATA lv_file_exists TYPE sap_bool.

    DATA ls_content LIKE LINE OF it_content.

    DATA: lx_file_access_error TYPE REF TO cx_sy_file_access_error,
          lx_file_close        TYPE REF TO cx_sy_file_close.

    IF iv_overwrite = abap_true.
      split_full_path(
        EXPORTING
          iv_full_path = iv_full_path
        IMPORTING
          ev_dirname   = lv_dirname ).

      IF directory_exists( lv_dirname ) <> abap_true.
        create_directory( lv_dirname ).
      ENDIF.
    ENDIF.

    lv_file_exists = file_exists( iv_full_path ).

    IF lv_file_exists = abap_true AND iv_overwrite = abap_false.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Cant overwrite file'.
    ENDIF.

    OPEN DATASET iv_full_path FOR OUTPUT IN BINARY MODE.

    IF sy-subrc <> 0.
      CONCATENATE 'Cant open' iv_full_path INTO lv_msg
          SEPARATED BY space.

      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = lv_msg.
    ENDIF.

    TRY.
        LOOP AT it_content INTO ls_content.
          TRANSFER ls_content TO iv_full_path.
        ENDLOOP.

      CATCH cx_sy_file_access_error INTO lx_file_access_error.
        lv_msg = lx_file_access_error->get_text( ).
        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

    TRY.
        CLOSE DATASET iv_full_path.
      CATCH cx_sy_file_close INTO lx_file_close.
        lv_msg = lx_file_close->get_text( ).

        RAISE EXCEPTION TYPE ycx_ni_file_operations
          EXPORTING
            iv_text = lv_msg.
    ENDTRY.

  ENDMETHOD.



ENDCLASS.
