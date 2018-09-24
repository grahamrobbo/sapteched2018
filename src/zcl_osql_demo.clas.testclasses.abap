CLASS ltc_test_osql DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES :
      BEGIN OF ty_customer_info,
        company_name     TYPE string,
        sum_gross_amount TYPE p LENGTH 15 DECIMALS 2,
      END OF ty_customer_info.
    TYPES : tt_customer_info TYPE STANDARD TABLE OF ty_customer_info.

    DATA:
      i_parm_values           TYPE if_osql_param_values_config=>ty_parameter_value_pairs,

      sales_order             TYPE STANDARD TABLE OF sepm_i_salesorder WITH EMPTY KEY,
      business_partner        TYPE STANDARD TABLE OF sepm_i_businesspartner WITH EMPTY KEY,
      product_info            TYPE STANDARD TABLE OF osqlfrwk_product_info,

      lt_product_info_result  TYPE STANDARD TABLE OF osqlfrwk_product_info,
      lt_customer_info_result TYPE tt_customer_info,

      class_object            TYPE REF TO cl_osql_comprehensive_demo.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    CLASS-DATA:
      environment TYPE REF TO if_osql_test_environment.

    METHODS:
      setup,
      check_with_no_sales_order           FOR TESTING,
      check_valid_so_but_no_info          FOR TESTING,
      check_valid_so_with_info            FOR TESTING.

ENDCLASS.


CLASS ltc_test_osql IMPLEMENTATION.


  METHOD class_setup.

**********************************************************************
*   Create doubles for the database artifacts which are used in the class
**********************************************************************

    " Provide the list of all the database artifacts which are used by the Open SQL statements in the Development Object Under Test.
    "
    " After the test environment creation step, doubles will get created for all the artifacts which are given in the list
    " and can be provided with test data for testing open sql.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
                                                                                 ( 'sepm_i_salesorder'   )
                                                                                 ( 'sepm_i_businesspartner'   )
                                                                                 ( 'osqlfrwk_product_info'       )
                                                                               )
    ).

  ENDMETHOD.

  METHOD class_teardown.
    "removes all doubles created as part of test session
    environment->destroy( ).
  ENDMETHOD.


  METHOD setup.
    CREATE OBJECT class_object.
    "clear all the doubles before the next test method execution
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD check_with_no_sales_order.

    "fails at lv_exists condition check as no data is present for sepm_i_salesorder and sepm_i_businesspartner

    "test get_product_info()
    "----------------------
    class_object->get_product_info( EXPORTING iv_customer_id = '10000404' IMPORTING et_product = lt_product_info_result ).
    cl_abap_unit_assert=>assert_initial( lt_product_info_result ).

    "test get_customer_info()
    "-----------------------
    class_object->get_customer_info( EXPORTING iv_customer_id = '10000404' IMPORTING et_customer_info = lt_customer_info_result ).
    cl_abap_unit_assert=>assert_initial( lt_product_info_result ).

  ENDMETHOD.

  METHOD check_valid_so_but_no_info.

    "test get_product_info()
    "----------------------

    "prepare test data
    sales_order = VALUE #( ( customeruuid = '10000404' SalesOrderBillingStatus = 'P' CreationDateTime = '201610101' TransactionCurrency  = 'EUR' ) ).
    "insert test data
    environment->insert_test_data( sales_order ).

    "prepare test data
    business_partner = VALUE #( ( businesspartneruuid = '10000404' CompanyName = 'SAP' Currency = 'EUR' businesspartner = '10000405' ) ).
    "insert test data
    environment->insert_test_data( business_partner ).

    "prepare test data
    product_info  = VALUE #( ( customer_id = '10000000' product_cnt = 8 product_name = 'Television' ) ).
    "prepare test data for CDS parameters
    i_parm_values = VALUE #( ( parameter_name = 'langu'          parameter_value = cl_abap_syst=>get_logon_language( ) )
                             ( parameter_name = 'fallback_langu' parameter_value = cl_abap_syst=>get_language( ) )     ).
    "insert test data
    environment->insert_test_data( i_data = product_info i_parameter_values = i_parm_values ).

    "Test
    class_object->get_product_info( EXPORTING iv_customer_id = '10000404' IMPORTING et_product = lt_product_info_result ).

    " lt_product_info_result is initial as there is no data present in the double of OsqlFrwk_Product_Info for the parameter '10000404'
    cl_abap_unit_assert=>assert_initial( lt_product_info_result ).

  ENDMETHOD.

  METHOD check_valid_so_with_info.

    "test get_product_info()
    "----------------------

    "provide test data
    sales_order = VALUE #( ( customeruuid = '10000404' SalesOrderBillingStatus = 'P' CreationDateTime = '201610101' TransactionCurrency = 'EUR' GrossAmountInTransacCurrency = '5777.77' ) ).
    "insert test data
    environment->insert_test_data( sales_order ).

    "prepare test data
    business_partner = VALUE #( ( businesspartneruuid = '10000404' CompanyName = 'SAP' Currency = 'EUR' businesspartner = '10000404' ) ).
    "insert test data
    environment->insert_test_data( business_partner ).

    "prepare test data
    product_info  = VALUE #( ( customer_id = '10000404' product_cnt = 8 product_name = 'Television' ) ).
    "prepare test data for CDS parameters
    i_parm_values = VALUE #( ( parameter_name = 'langu'          parameter_value = cl_abap_syst=>get_logon_language( ) )
                             ( parameter_name = 'fallback_langu' parameter_value = cl_abap_syst=>get_language( ) )     ).
    "insert test data
    environment->insert_test_data( i_data = product_info i_parameter_values = i_parm_values ).

    "Test
    class_object->get_product_info( EXPORTING iv_customer_id = '10000404' IMPORTING et_product = lt_product_info_result ).
    "Assert
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_product_info_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = lt_product_info_result[ 1 ]-product_cnt ).


    "test get_customer_info()
    "-----------------------

    "Test
    class_object->get_customer_info( EXPORTING iv_customer_id = '10000404' IMPORTING et_customer_info = lt_customer_info_result ).
    "Assert
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_customer_info_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SAP' act = lt_customer_info_result[ 1 ]-company_name ).

  ENDMETHOD.

ENDCLASS.
