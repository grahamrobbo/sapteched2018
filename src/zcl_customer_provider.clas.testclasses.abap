CLASS lcl_customer_provider_test DEFINITION for testing
    duration short
    risk level harmless.

  PRIVATE SECTION.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS check_valid_so_with_info FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS lcl_customer_provider_test IMPLEMENTATION.

  METHOD class_setup.
    "For parameter i_dependency_list, specify the list of dependencies for which doubles are to be created
    "For parameter keyfields provide abap_true to retain keyfields of the dependencies while creating doubles. If set to abap_false, keyfields are discarded.
    "environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'SNWD_BPA'   )
    "                                                                            ( 'SNWD_AD'   ) )
    "                                              ).
  ENDMETHOD.

  METHOD class_teardown.
    "environment->destroy( ).
  ENDMETHOD.

  "Fixture method setup is executed once before each test method execution
  METHOD setup.
    "environment->clear_doubles( ).
  ENDMETHOD.

METHOD check_valid_so_with_info.
*    "Create an instance of type 'SEPM_I_BusinessPartner' and fill test data.
*    business_partner = VALUE #(  ( businesspartneruuid = '10000404' CompanyName = 'SAP' Currency = 'EUR' businesspartner = '10000404' ) ).
*    "Insert test data to the double
*    environment->insert_test_data( business_partner ).
*    “Create an instance of type 'SEPM_I_SalesOrder' and fill test data.
*    sales_order = VALUE #( ( customeruuid = '10000404' SalesOrderBillingStatus = 'P' CreationDateTime = '201610101' TransactionCurrency = 'EUR' GrossAmountInTransacCurrency = '5777.77' ) ).
*    “Insert test data to the double
*    environment->insert_test_data( sales_order ).
    ...
ENDMETHOD.

ENDCLASS.
