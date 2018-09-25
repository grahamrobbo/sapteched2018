CLASS ltc_aunit_demo DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    DATA m_cut TYPE REF TO zcl_aunit_demo.

    METHODS setup RAISING cx_static_check.

    METHODS teardown.

    METHODS check_value        FOR TESTING RAISING cx_static_check.
    METHODS check_add_value    FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_aunit_demo IMPLEMENTATION.

  METHOD setup.
    m_cut = NEW zcl_aunit_demo( 3 ).
  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

  METHOD check_value.
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_value(  )
                                        exp = 3 ).
  ENDMETHOD.

  METHOD check_add_value.
    m_cut->add(  2 ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_value(  )
                                        exp = 5 ).
  ENDMETHOD.

ENDCLASS.
