CLASS ycx_ni_file_operations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM ycx_ni_common.

  PUBLIC SECTION.

    "! @parameter textid | Text id
    "! @parameter previous | Previous
    "! @parameter iv_text | Message text
    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !iv_text  TYPE string OPTIONAL .
  PROTECTED SECTION.

    METHODS get_default_text REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCX_NI_FILE_OPERATIONS IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous
        iv_text  = iv_text.
  ENDMETHOD.


  METHOD get_default_text.
    rv_text = 'The operation failed' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
