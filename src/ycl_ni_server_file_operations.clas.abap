"! Helper class for file server manipulation
CLASS ycl_ni_server_file_operations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_file_list TYPE TABLE OF epsfili WITH DEFAULT KEY.
    TYPES: tt_file_content TYPE TABLE OF string WITH DEFAULT KEY.

    "! Open file content
    "! @parameter iv_full_path | Full filename with path
    "! @parameter rt_data | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS read_file
      IMPORTING
                iv_full_path   TYPE string
      RETURNING VALUE(rt_data) TYPE tt_file_content
      RAISING   ycx_ni_file_operations.

    "! Write content to file
    "! @parameter iv_full_path | Full filename with path
    "! @parameter it_data | File content
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS write_file
      IMPORTING
                iv_full_path TYPE string
                it_data      TYPE tt_file_content
                iv_overwrite TYPE sap_bool OPTIONAL
      RAISING   ycx_ni_file_operations.

    "! List all files in a given directory path
    "! @parameter iv_dirname | Full filename with path
    "! @parameter rt_files | List of filenames
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS list_files
      IMPORTING
                iv_dirname      TYPE string
      RETURNING VALUE(rt_files) TYPE tt_file_list
      RAISING   ycx_ni_file_operations.

    "! Copy file
    "! @parameter iv_source_full_path | Source full filename with path
    "! @parameter iv_dest_full_path | Destination full filename with path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS copy_file
      IMPORTING
                iv_source_full_path TYPE string
                iv_dest_full_path   TYPE string
      RAISING   ycx_ni_file_operations.

    "! Delete file
    "! @parameter iv_full_path | Full filename with path to be deleted
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS delete_file
      IMPORTING
                iv_full_path TYPE string
      RAISING   ycx_ni_file_operations.

    "! Move file
    "! @parameter iv_source_full_path | Source full filename with path
    "! @parameter iv_dest_full_path | Destination full filename with path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS move_file
      IMPORTING
                iv_source_full_path TYPE string
                iv_dest_full_path   TYPE string
      RAISING   ycx_ni_file_operations.

  PROTECTED SECTION.
    "! Handle standard messages returned by sy-subrc on function calls
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS raise_comm_error_from_sy
      RAISING ycx_ni_file_operations.

    "! Split full path into directory path and filename
    "! @parameter iv_full_path | Full path
    "! @parameter ev_dirname | Directory path
    "! @parameter ev_filename | Filename
    METHODS split_full_path
      IMPORTING
        iv_full_path TYPE string
      EXPORTING
        ev_dirname   TYPE string
        ev_filename  TYPE string.


    "! Check if the path of directory is valid
    "! @parameter iv_dirname | Directory path
    "! @parameter rv_valid | Is valid?
    METHODS directory_exists
      IMPORTING
                iv_dirname      TYPE string
      RETURNING VALUE(rv_valid) TYPE sap_bool.

    "! Check if the file exists
    "! @parameter iv_full_path | Full path of file
    "! @parameter rv_valid | Is valid?
    METHODS file_exists
      IMPORTING
                iv_full_path    TYPE string
      RETURNING VALUE(rv_valid) TYPE sap_bool.

    "! Create the given directory
    "! @parameter iv_dirname | Directory path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS create_directory
      IMPORTING
                iv_dirname TYPE string
      RAISING   ycx_ni_file_operations.

  PRIVATE SECTION.

    "! Create the given directory !!! INTERNAL USE ONLY. NEVER USE IR. I MEAN IT.
    "! @parameter iv_dirname | Directory path
    "! @raising ycx_ni_file_operations | Operation Failed
    METHODS delete_directory
      IMPORTING
                iv_recursively TYPE sap_bool OPTIONAL
                iv_dirname     TYPE string
      RAISING   ycx_ni_file_operations.

ENDCLASS.



CLASS ycl_ni_server_file_operations IMPLEMENTATION.

  METHOD read_file.

    cl_rsan_ut_appserv_file_reader=>appserver_file_read(
       EXPORTING
         i_filename   = iv_full_path "type string
       CHANGING
         c_data_tab   = rt_data " type rsanm_file_table
       EXCEPTIONS
         open_failed  = 1
         read_failed  = 2
         close_failed = 3
         OTHERS       = 4  ).

  ENDMETHOD.

  METHOD write_file.

    IF iv_overwrite = abap_true.

      me->split_full_path(
        EXPORTING
          iv_full_path = iv_full_path
        IMPORTING
          ev_dirname   = DATA(lv_dirname)
      ).

      IF NOT me->directory_exists( lv_dirname ).
        me->create_directory( lv_dirname ).
      ENDIF.

    ENDIF.

    cl_rsan_ut_appserv_file_writer=>appserver_file_write(
           EXPORTING
             i_filename      = iv_full_path
             i_overwrite     = iv_overwrite
             i_data_tab      = it_data
           EXCEPTIONS
             open_failed     = 1
             write_failed    = 2
             close_failed    = 3
             OTHERS          = 4 ).

    IF sy-subrc IS NOT INITIAL.
      me->raise_comm_error_from_sy( ).
    ENDIF.

  ENDMETHOD.

  METHOD list_files.

    DATA lv_directory TYPE epsdirnam.

    lv_directory = iv_dirname.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = lv_directory
      TABLES
        dir_list               = rt_files
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    IF sy-subrc IS NOT INITIAL.
      me->raise_comm_error_from_sy( ).
    ENDIF.


  ENDMETHOD.

  METHOD copy_file.

    DATA lt_file_contents TYPE TABLE OF string.

    lt_file_contents = me->read_file( iv_source_full_path ).

    me->write_file(
    EXPORTING
        iv_full_path = iv_dest_full_path
        it_data = lt_file_contents
    ).

  ENDMETHOD.

  METHOD delete_file.

    DATA lv_filename TYPE epsfilnam.
    DATA lv_dirname TYPE epsdirnam.

    FIND REGEX '[^\\/:*?"<>|\r\n]+$' IN iv_full_path RESULTS DATA(ls_find_results).
    lv_filename = iv_full_path+ls_find_results-offset(ls_find_results-length).

    lv_dirname = iv_full_path.
    REPLACE REGEX '[^\\/:*?"<>|\r\n]+$' IN lv_dirname WITH space.

    CALL FUNCTION 'EPS_DELETE_FILE'
      EXPORTING
        file_name              = lv_filename
        dir_name               = lv_dirname
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        build_path_failed      = 5
        delete_failed          = 6
        OTHERS                 = 7.

    IF sy-subrc IS NOT INITIAL.
      me->raise_comm_error_from_sy( ).
    ENDIF.

  ENDMETHOD.

  METHOD move_file.

    me->copy_file(
      iv_source_full_path = iv_source_full_path
      iv_dest_full_path = iv_dest_full_path
    ).

    me->delete_file( iv_source_full_path ).

  ENDMETHOD.

  METHOD raise_comm_error_from_sy.
    DATA: lv_error_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_error_msg.
    RAISE EXCEPTION TYPE ycx_ni_file_operations
      EXPORTING
        iv_text = |Operation error: { lv_error_msg }| ##NO_TEXT.
  ENDMETHOD.

  METHOD split_full_path.

    FIND REGEX '[^\\/:*?"<>|\r\n]+$' IN iv_full_path RESULTS DATA(ls_find_results).
    ev_filename = iv_full_path+ls_find_results-offset(ls_find_results-length).
    ev_dirname = iv_full_path.
    REPLACE REGEX '[^\\/:*?"<>|\r\n]+$' IN ev_dirname WITH space.

  ENDMETHOD.

  METHOD file_exists.

    me->split_full_path(
        EXPORTING
            iv_full_path = iv_full_path
        IMPORTING
            ev_dirname = DATA(lv_dirname)
            ev_filename = DATA(lv_filename) ).

    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = lv_dirname && space
        filname                     = lv_filename && space
      EXCEPTIONS
        pfl_dir_not_exist           = 1              " Directory does not exist
        pfl_permission_denied       = 2              " No write authorization for directory
        pfl_cant_build_dataset_name = 3              " Temporary file cannot be generated
        pfl_file_not_exist          = 4
        pfl_authorization_missing   = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      rv_valid = abap_true.
    ENDIF.


  ENDMETHOD.

  METHOD directory_exists.

    DATA(lv_dirname) = CONV btch0000-text80( iv_dirname ).

    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = lv_dirname
      EXCEPTIONS
        pfl_dir_not_exist           = 1              " Directory does not exist
        pfl_permission_denied       = 2              " No write authorization for directory
        pfl_cant_build_dataset_name = 3              " Temporary file cannot be generated
        pfl_file_not_exist          = 4
        pfl_authorization_missing   = 5
        OTHERS                      = 6.
    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create_directory.
    DATA lv_command TYPE char255.
    DATA lt_result TYPE TABLE OF char255.

    CALL FUNCTION 'AUTHORITY_CHECK_C_FUNCTION'
      EXPORTING
        activity         = 'CALL'
        function         = 'SYSTEM'
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      me->raise_comm_error_from_sy( ).
    ENDIF.

    lv_command = |mkdir { iv_dirname }|.

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

    CALL FUNCTION 'AUTHORITY_CHECK_C_FUNCTION'
      EXPORTING
        activity         = 'CALL'
        function         = 'SYSTEM'
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      me->raise_comm_error_from_sy( ).
    ENDIF.

    IF iv_recursively = abap_true.
      lv_command = |rm -r { iv_dirname }|.
    ELSE.
      lv_command = |rm -d { iv_dirname }|.
    ENDIF.

    CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command
                  ID 'TAB'     FIELD lt_result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_ni_file_operations
        EXPORTING
          iv_text = 'Directory not deleted'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
