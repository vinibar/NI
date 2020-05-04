CLASS ycl_ni_exception_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <em>NOTE: Forked from FM RSAN_MDL_EXCEPTION_TO_SYMSG</em>
      "! Convert exception to symsg
      "! @parameter io_exception | Exception object to be converted
      exception_to_symsg
        IMPORTING
          io_exception TYPE REF TO cx_root.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_ni_exception_util IMPLEMENTATION.

  METHOD exception_to_symsg.

    TYPES:
      BEGIN OF lty_msg,
        v1 TYPE msgv1,
        v2 TYPE msgv2,
        v3 TYPE msgv3,
        v4 TYPE msgv4,
      END OF lty_msg.

    DATA ls_msg TYPE lty_msg.

    DATA: lo_typedescr    TYPE REF TO cl_abap_typedescr,
          lv_classname    TYPE seoclsname,
          lo_object_descr TYPE REF TO cl_oo_object,
          ls_attribute    TYPE vseoattrib.

    DATA lv_dummy TYPE c LENGTH 100.

    " Fill message fields v1, v2 and v3 with the exception text
    ls_msg = io_exception->get_text( ).

    " V4 must be clear 'cause it will store only technical id or absolute class name
    CLEAR ls_msg-v4.

    lo_typedescr  = cl_abap_typedescr=>describe_by_object_ref( io_exception ).
    lv_classname = lo_typedescr->get_relative_name( ).

    " Get technical ID
    TRY.
        lo_object_descr = cl_oo_object=>get_instance( lv_classname ).

        LOOP AT lo_object_descr->attributes INTO ls_attribute
             WHERE attvalue CS io_exception->textid.
          CONCATENATE lv_classname '=>' ls_attribute-cmpname INTO ls_msg-v4.
          EXIT.
        ENDLOOP.

      CATCH cx_class_not_existent.
        CLEAR ls_msg-v4.
    ENDTRY.

    " If technical id is still empty, use absolute classname instead
    IF ls_msg-v4 IS INITIAL.
      ls_msg-v4 = lo_typedescr->absolute_name.
    ENDIF.

    " If message text is empty (i.e. not translated), display the technical identifier
    IF ls_msg-v1 IS INITIAL.
      ls_msg = ls_msg-v4.
    ENDIF.

    " Some variables for debugging purposes
    DATA: lv_program_name TYPE syrepid,
          lv_include_name TYPE syrepid,
          lv_source_line  TYPE i.

    io_exception->get_source_position( IMPORTING program_name = lv_program_name
                                                 include_name = lv_include_name
                                                 source_line  = lv_source_line ).


    " Finally, write message to sy-fields
    MESSAGE ID 'YNI' TYPE 'E' NUMBER '000'
            WITH ls_msg-v1 ls_msg-v2 ls_msg-v3 ls_msg-v4 INTO lv_dummy.

  ENDMETHOD.

ENDCLASS.
