CLASS ltc_aunit_demo DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    DATA m_cut TYPE REF TO zcl_aunit_demo.

    METHODS given_value
      IMPORTING
        value TYPE int4.

    METHODS value_should_be
      IMPORTING
        value TYPE int4.

    METHODS check_value        FOR TESTING RAISING cx_static_check.
    METHODS check_add_value    FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_aunit_demo IMPLEMENTATION.

  METHOD given_value.
    m_cut = NEW zcl_aunit_demo( value ).
  ENDMETHOD.

  METHOD value_should_be.
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_value(  )
                                        exp = value ).
  ENDMETHOD.

  METHOD check_add_value.
    given_value( 6 ).

    m_cut->add(  4 ).

    value_should_be( 10 ).
  ENDMETHOD.

  METHOD check_value.
    given_value( 42 ).

    value_should_be( 42 ).
  ENDMETHOD.


ENDCLASS.
