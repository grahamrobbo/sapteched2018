CLASS lcl_customer_provider_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

    CLASS-DATA osql_doubles TYPE REF TO if_osql_test_environment.

    CLASS-DATA mock_customer TYPE zif_customer_provider=>customer_type.

    DATA m_cut TYPE REF TO zif_customer_provider.

    METHODS setup RAISING cx_static_check.

    METHODS teardown.

    METHODS check_not_found  FOR TESTING RAISING cx_static_check.
    METHODS check_all_properties  FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS lcl_customer_provider_test IMPLEMENTATION.
  METHOD class_constructor.

    mock_customer = VALUE #(
            node_key = '0123456789ABCDEF'
            bp_id = 'TESTY'
            company_name = 'The Test Company'
            street = '12 MyStreet Lane'
            city = 'My City'
            postal_code = '12345'
            country = 'ZZ'
            country_text = 'My Country'
    ).
  ENDMETHOD.

  METHOD class_setup.
    osql_doubles = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'snwd_bpa' ) ( 'snwd_ad' ) ( 't005t' ) ) ).

    "prepare test data
    DATA: business_partner  TYPE snwd_bpa,
          business_partners TYPE TABLE OF snwd_bpa WITH EMPTY KEY,
          address           TYPE snwd_ad,
          addresses         TYPE TABLE OF snwd_ad WITH EMPTY KEY,
          countries         TYPE TABLE OF t005t WITH EMPTY KEY.

    MOVE-CORRESPONDING mock_customer TO business_partner.
    business_partner-address_guid = business_partner-node_key.
    business_partners = VALUE #( ( business_partner ) ).
    osql_doubles->insert_test_data( business_partners ).

    MOVE-CORRESPONDING mock_customer TO address.
    address-node_key = business_partner-address_guid.
    addresses = VALUE #(  (  address ) ).
    osql_doubles->insert_test_data(  addresses ).

    countries = VALUE #( ( spras = sy-langu land1 = mock_customer-country landx50 = mock_customer-country_text ) ).
    osql_doubles->insert_test_data(  countries ).

  ENDMETHOD.

  METHOD class_teardown.
    osql_doubles->clear_doubles(  ).
    osql_doubles->destroy( ).
  ENDMETHOD.

  METHOD setup.
    m_cut = zcl_customer_provider_factory=>get_customer_provider( mock_customer-node_key ).
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD check_all_properties.
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_node_key(  )
                                        exp = mock_customer-node_key ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_bp_id(  )
                                        exp = mock_customer-bp_id ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_company_name(  )
                                        exp = mock_customer-company_name ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_street(  )
                                        exp = mock_customer-street ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_city(  )
                                        exp = mock_customer-city ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_postal_code(  )
                                        exp = mock_customer-postal_code ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_country(  )
                                        exp = mock_customer-country ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_country_text(  )
                                        exp = mock_customer-country_text ).

  ENDMETHOD.

  METHOD check_not_found.
    TRY.
        zcl_customer_provider_factory=>get_customer_provider( cl_system_uuid=>create_uuid_x16_static( ) ).

      CATCH cx_abap_invalid_value INTO DATA(cx).
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>fail(
      EXPORTING
        msg    = 'CX_ABAP_INVALID_VALUE Exception did not occur as expected'
*        level  = if_aunit_constants=>critical    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        quit   = if_aunit_constants=>method    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*        detail = 'This check did not trigger the expected exception'    " Further Description
    ).

  ENDMETHOD.

ENDCLASS.
