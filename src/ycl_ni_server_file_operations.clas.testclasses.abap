CLASS lcl_test_file_operations DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      split_full_path FOR TESTING RAISING cx_static_check,
      directory_exists FOR TESTING RAISING cx_static_check,
      create_directory FOR TESTING RAISING cx_static_check,
      write_file FOR TESTING RAISING cx_static_check,
      delete_file FOR TESTING RAISING cx_static_check,
      read_file FOR TESTING RAISING cx_static_check,
      list_files FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_test_file_operations IMPLEMENTATION.

  METHOD split_full_path.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).

    lo_file_operations->split_full_path(
        EXPORTING
        iv_full_path = `/tmp/test.txt`
        IMPORTING
        ev_dirname = DATA(lv_dirname)
        ev_filename = DATA(lv_filename) ).

    cl_abap_unit_assert=>assert_equals( msg = 'Fullpath not properly splitted' exp = '/tmp/' act = lv_dirname ).
    cl_abap_unit_assert=>assert_equals( msg = 'Fullpath not properly splitted' exp = 'test.txt' act = lv_filename ).

  ENDMETHOD.

  METHOD directory_exists.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).

    IF NOT lo_file_operations->directory_exists( `/tmp` ).
      cl_abap_unit_assert=>fail(
        msg    = 'Valid path return invalid' ).
    ENDIF.


    IF lo_file_operations->directory_exists( `/tmp123__` ).
      cl_abap_unit_assert=>fail(
        msg    = 'Valid path return invalid' ).
    ENDIF.


  ENDMETHOD.

  METHOD create_directory.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).

    TRY.
        lo_file_operations->create_directory( iv_dirname = '/tmp/createdbytest' ).
      CATCH ycx_ni_file_operations.
        cl_abap_unit_assert=>fail(
            msg    = 'Directory not created' ).
    ENDTRY.

    TRY.
        lo_file_operations->delete_directory( iv_dirname = '/tmp/createdbytest' ).
      CATCH ycx_ni_file_operations.
    ENDTRY.


  ENDMETHOD.

  METHOD write_file.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).

    DATA(lt_content) = VALUE lo_file_operations->tt_file_content(
        ( `line 1` )
        ( `line 2` )
    ).

    TRY.
        lo_file_operations->write_file(
          EXPORTING
            iv_full_path = '/tmp/unittest.txt'
            it_data      = lt_content
            iv_overwrite = abap_true
        ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
        cl_abap_unit_assert=>fail(
          msg    = 'File not created' ).
        RETURN.
    ENDTRY.

    TRY.
        lo_file_operations->delete_file( iv_full_path = '/tmp/unittest.txt'  ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
    ENDTRY.


  ENDMETHOD.

  METHOD delete_file.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).
    DATA(lt_content) = VALUE lo_file_operations->tt_file_content(
        ( `line 1` )
        ( `line 2` )
    ).

    TRY.
        lo_file_operations->write_file(
          EXPORTING
            iv_full_path = '/tmp/unittest.txt'
            it_data      = lt_content
            iv_overwrite = abap_true
        ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
    ENDTRY.


    TRY.
        lo_file_operations->delete_file( iv_full_path = '/tmp/unittest.txt'  ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
        cl_abap_unit_assert=>fail(
          msg    = 'File not deleted' ).
    ENDTRY.

  ENDMETHOD.

  METHOD read_file.

    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).
    DATA(lt_content) = VALUE lo_file_operations->tt_file_content(
        ( `line 1` )
        ( `line 2` )
    ).

    TRY.
        lo_file_operations->write_file(
          EXPORTING
            iv_full_path = '/tmp/unittest.txt'
            it_data      = lt_content
            iv_overwrite = abap_true
        ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
    ENDTRY.

    TRY.
        lt_content = lo_file_operations->read_file( '/tmp/unittest.txt' ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
        cl_abap_unit_assert=>fail(
      msg    = 'Cant open the file' ).
    ENDTRY.

    READ TABLE lt_content INTO DATA(ls_content) INDEX 1.
    IF sy-subrc <> 0 OR ls_content <> `line 1`.
      cl_abap_unit_assert=>fail(
msg    = 'File opened but invalid content' ).
    ENDIF.


  ENDMETHOD.

  METHOD list_files.


    DATA(lo_file_operations) = NEW ycl_ni_server_file_operations( ).

    DATA(lt_content) = VALUE lo_file_operations->tt_file_content(
        ( `line 1` )
        ( `line 2` )
    ).

    TRY.
        lo_file_operations->write_file(
          EXPORTING
            iv_full_path = '/tmp/dirUnitTest/unittest.txt'
            it_data      = lt_content
            iv_overwrite = abap_true
        ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
    ENDTRY.

    TRY.
        DATA(lt_list) = lo_file_operations->list_files( '/tmp/dirUnitTest' ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
        cl_abap_unit_assert=>fail(
            msg    = 'Cant list files in directory' ).
    ENDTRY.

    READ TABLE lt_list INTO DATA(ls_list) INDEX 1.
    IF sy-subrc <> 0 OR ls_list-name <> 'unittest.txt'.
      cl_abap_unit_assert=>fail(
        msg    = 'Cant list files in directory' ).
    ENDIF.

    TRY.
        lo_file_operations->delete_directory(
          EXPORTING
            iv_recursively = abap_true
            iv_dirname     = '/tmp/dirUnitTest'
        ).
      CATCH ycx_ni_file_operations. " Exception Handler for File Operations
    ENDTRY.



  ENDMETHOD.

ENDCLASS.
