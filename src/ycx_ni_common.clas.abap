CLASS ycx_ni_common DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !iv_text  TYPE string OPTIONAL .
  PROTECTED SECTION.

    DATA mv_text TYPE string.

    METHODS get_default_text
      RETURNING
        VALUE(rv_text) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ycx_ni_common IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.

    mv_text = iv_text.
  ENDMETHOD.


  METHOD get_default_text.
    rv_text = 'An error ocurred.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
