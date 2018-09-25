CLASS zcl_aunit_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING !iv_value TYPE int4.
    METHODS get_value
      RETURNING
        VALUE(rv_value) TYPE int4.
    METHODS add
      IMPORTING !iv_add TYPE int4.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: value TYPE int4.
ENDCLASS.

CLASS zcl_aunit_demo IMPLEMENTATION.
  METHOD constructor.
    value = iv_value.
  ENDMETHOD.

  METHOD get_value.
    rv_value = value.
  ENDMETHOD.

  METHOD add.
    value = value + iv_add.
  ENDMETHOD.

ENDCLASS.
