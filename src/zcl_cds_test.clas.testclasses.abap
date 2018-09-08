CLASS ltc_demo_2 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_exp_result,
        so_id            TYPE cdsfrwk_so_items_by_taxrate-so_id,
        currency_code    TYPE cdsfrwk_so_items_by_taxrate-currency_code,
        sum_gross_amount TYPE cdsfrwk_so_items_by_taxrate-sum_gross_amount,
        tax_rate         TYPE cdsfrwk_so_items_by_taxrate-tax_rate,
      END OF ty_exp_result,
      ty_exp_results TYPE STANDARD TABLE OF ty_exp_result WITH EMPTY KEY.

    CLASS-DATA:
      environment        TYPE REF TO if_cds_test_environment.
    CLASS-METHODS:
      class_setup
        RAISING
          cx_static_check,
      class_teardown.
    DATA:
      act_results       TYPE STANDARD TABLE OF cdsfrwk_so_items_by_taxrate WITH EMPTY KEY,
      exp_results       TYPE ty_exp_results,
      test_data         TYPE REF TO if_cds_test_data,
      sales_orders      TYPE STANDARD TABLE OF snwd_so WITH EMPTY KEY,
      sales_order_items TYPE STANDARD TABLE OF cdsfrwk_demo_1 WITH EMPTY KEY.
    METHODS:
      setup RAISING cx_static_check,
      assert_so_items_by_taxrate
        IMPORTING
          exp_results TYPE ty_exp_results,
      cuco_1_taxrate_1_item_1_ok FOR TESTING RAISING cx_static_check,
      cuco_1_taxrate_1_item_2_ok FOR TESTING RAISING cx_static_check,
      cuco_1_taxrate_2_item_2_ok FOR TESTING RAISING cx_static_check,
      cuco_2_taxrate_1_item_2_ok FOR TESTING RAISING cx_static_check,
      so_head_does_not_exist_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_demo_2 IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'CdsFrwk_So_Items_By_TaxRate' ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD cuco_1_taxrate_1_item_1_ok.
    "Given
    "Sales Orders
    sales_orders = VALUE #(  ( client = sy-mandt node_key = '01' so_id = 'ID' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_orders ).

    DATA(sales_orders_stub) = environment->get_double( i_name = 'SNWD_SO' ).
    sales_orders_stub->insert( test_data ).

    "Given
    "Sales Order Items
    sales_order_items = VALUE #( ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '1' tax_rate = '19.00' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_order_items ).

    DATA(sales_order_items_stub) = environment->get_double(  i_name = 'CdsFrwk_DEMO_1' ).
    sales_order_items_stub->insert( test_data ).

    "Test
    SELECT * FROM cdsfrwk_so_items_by_taxrate INTO TABLE @act_results.

    "Verify
    exp_results = VALUE #( ( so_id = 'ID' currency_code = 'EUR' sum_gross_amount = '1' tax_rate = '19.00' ) ).
    assert_so_items_by_taxrate( exp_results = exp_results ).

  ENDMETHOD.


  METHOD cuco_1_taxrate_1_item_2_ok.
    "Given
    "Sales Orders
    sales_orders = VALUE #(  ( client = sy-mandt node_key = '01' so_id = 'ID' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_orders ).

    DATA(sales_orders_stub) = environment->get_double( i_name = 'SNWD_SO' ).
    sales_orders_stub->insert( test_data ).

    "Given
    "Sales Order Items
    sales_order_items = VALUE #( ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '1' tax_rate = '19.00' )
                                 ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '2' tax_rate = '19.00' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_order_items ).

    DATA(sales_order_items_stub) = environment->get_double(  i_name = 'CdsFrwk_DEMO_1' ).
    sales_order_items_stub->insert( test_data ).

    "Test
    SELECT * FROM cdsfrwk_so_items_by_taxrate INTO TABLE @act_results.
    "Verify
    exp_results = VALUE #( ( so_id = 'ID' currency_code = 'EUR' sum_gross_amount = '3' tax_rate = '19.00' ) ).
    assert_so_items_by_taxrate( exp_results = exp_results ).

  ENDMETHOD.


  METHOD cuco_1_taxrate_2_item_2_ok.
    "Given
    "Sales Orders
    sales_orders = VALUE #(  ( client = sy-mandt node_key = '01' so_id = 'ID' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_orders ).

    DATA(sales_orders_stub) = environment->get_double( i_name = 'SNWD_SO' ).
    sales_orders_stub->insert( test_data ).

    "Given
    "Sales Order Items
    sales_order_items = VALUE #( ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '1' tax_rate = '19.00' )
                                 ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '2' tax_rate = '7.00' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_order_items ).

    DATA(sales_order_items_stub) = environment->get_double(  i_name = 'CdsFrwk_DEMO_1' ).
    sales_order_items_stub->insert( test_data ).

    "Test
    SELECT * FROM cdsfrwk_so_items_by_taxrate INTO TABLE @act_results.

    "Verify
    exp_results = VALUE #( ( so_id = 'ID' currency_code = 'EUR' sum_gross_amount = '1' tax_rate = '19.00' )
                           ( so_id = 'ID' currency_code = 'EUR' sum_gross_amount = '2' tax_rate = '7.00' ) ).
    assert_so_items_by_taxrate( exp_results = exp_results ).

  ENDMETHOD.



  METHOD cuco_2_taxrate_1_item_2_ok.
    "Given
    "Sales Orders
    sales_orders = VALUE #(  ( client = sy-mandt node_key = '01' so_id = 'ID' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_orders ).

    DATA(sales_orders_stub) = environment->get_double( i_name = 'SNWD_SO' ).
    sales_orders_stub->insert( test_data ).

    "Given
    "Sales Order Items
    sales_order_items = VALUE #( ( mandt = sy-mandt so_guid = '01' currency_code = 'USD' gross_amount = '1' tax_rate = '17.00' )
                                 ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '2' tax_rate = '7.00' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_order_items ).

    DATA(sales_order_items_stub) = environment->get_double(  i_name = 'CdsFrwk_DEMO_1' ).
    sales_order_items_stub->insert( test_data ).

    "Test
    SELECT * FROM cdsfrwk_so_items_by_taxrate INTO TABLE @act_results.

    "Verify
    exp_results = VALUE #( ( so_id = 'ID' currency_code = 'USD' sum_gross_amount = '1' tax_rate = '17.00' )
                           ( so_id = 'ID' currency_code = 'EUR' sum_gross_amount = '2' tax_rate = '7.00' ) ).
    assert_so_items_by_taxrate( exp_results = exp_results ).

  ENDMETHOD.

  METHOD so_head_does_not_exist_ok.

    "Given
    "Sales Order Items
    sales_order_items = VALUE #( ( mandt = sy-mandt so_guid = '01' currency_code = 'EUR' gross_amount = '1' tax_rate = '19.00' ) ).
    test_data = cl_cds_test_data=>create( i_data =  sales_order_items ).

    DATA(sales_order_items_stub) = environment->get_double(  i_name = 'CdsFrwk_DEMO_1' ).
    sales_order_items_stub->insert( test_data ).

    "Test
    SELECT * FROM cdsfrwk_so_items_by_taxrate INTO TABLE @act_results.

    "Verify
    exp_results = VALUE #( ( so_id = '9999999999' currency_code = 'EUR' sum_gross_amount = '1' tax_rate = '19.00' ) ).
    assert_so_items_by_taxrate( exp_results = exp_results ).

  ENDMETHOD.


  METHOD assert_so_items_by_taxrate.

    cl_abap_unit_assert=>assert_equals(
        act                  =  lines( act_results )
        exp                  =  lines( exp_results ) ).

    LOOP AT exp_results ASSIGNING FIELD-SYMBOL(<exp_result>).

      READ TABLE act_results WITH KEY so_id = <exp_result>-so_id
                                      currency_code = <exp_result>-currency_code
                                      tax_rate = <exp_result>-tax_rate
                                      ASSIGNING FIELD-SYMBOL(<act_result>).
      cl_abap_unit_assert=>assert_subrc(
          exp = 0
          act = sy-subrc
          msg = |so_id '{ <exp_result>-so_id }', currency_code '{ <exp_result>-currency_code }', tax_rate '{ <exp_result>-tax_rate }' not found| ).

      cl_abap_unit_assert=>assert_equals(
         exp = <exp_result>-sum_gross_amount
         act = <act_result>-sum_gross_amount
         msg = |so_id '{ <exp_result>-so_id }', currency_code '{ <exp_result>-currency_code }', tax_rate '{ <exp_result>-tax_rate }' wrong sum_gross_amount| ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
