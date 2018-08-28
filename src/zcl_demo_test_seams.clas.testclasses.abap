*"* use this source file for your ABAP unit test classes

CLASS test_demo_test_seam DEFINITION FOR TESTING
                          RISK LEVEL HARMLESS
                          DURATION SHORT
                          FINAL.
  PRIVATE SECTION.
    METHODS: setup,
      test_not_found FOR TESTING RAISING cx_static_check,
      test_modification_fail FOR TESTING RAISING cx_static_check,
      test_change_price FOR TESTING RAISING cx_static_check,
      test_change_90 FOR TESTING RAISING cx_static_check,
      test_authorisation_check FOR TESTING RAISING cx_static_check,
      invoke_and_assert IMPORTING exp TYPE i.
ENDCLASS.

CLASS test_demo_test_seam IMPLEMENTATION.

  METHOD setup.
**********************************************************************
* Failing test injection in setup method for every test seam         *
*                                                                    *
* This makes it easy to recognise when I forget to define a test     *
* injection in a test                                                *
**********************************************************************

    TEST-INJECTION authorisation.
      cl_abap_unit_assert=>fail( msg = 'Redefine me' ).
    END-TEST-INJECTION.

    TEST-INJECTION selection.
      cl_abap_unit_assert=>fail( msg = 'Redefine me' ).
    END-TEST-INJECTION.

    TEST-INJECTION modification.
      cl_abap_unit_assert=>fail( msg = 'Redefine me' ).
    END-TEST-INJECTION.
**********************************************************************

    TEST-INJECTION authorisation.
    END-TEST-INJECTION.

    TEST-INJECTION selection.
      wa-price = 100.
    END-TEST-INJECTION.

    TEST-INJECTION modification.
    END-TEST-INJECTION.

  ENDMETHOD.

  METHOD test_authorisation_check.

    TEST-INJECTION authorisation.
      sy-subrc = 4.
    END-TEST-INJECTION.

    NEW zcl_demo_test_seams( )->change_price(
      EXPORTING
         carrid = '   '
         connid = '0000'
         fldate = '00000000'
         factor = 90
       IMPORTING new_price = DATA(new_price) ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       exp = -3
       act = new_price  ).

  ENDMETHOD.

  METHOD test_not_found.

    TEST-INJECTION selection.
      sy-subrc = 4.
    END-TEST-INJECTION.

    NEW zcl_demo_test_seams( )->change_price(
      EXPORTING
         carrid = '   '
         connid = '0000'
         fldate = '00000000'
         factor = 90
       IMPORTING new_price = DATA(new_price) ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       exp = -1
       act = new_price  ).

  ENDMETHOD.

  METHOD test_modification_fail.

    TEST-INJECTION modification.
      sy-subrc = 4.
    END-TEST-INJECTION.

    NEW zcl_demo_test_seams( )->change_price(
      EXPORTING
         carrid = '   '
         connid = '0000'
         fldate = '00000000'
         factor = 90
       IMPORTING new_price = DATA(new_price) ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       exp = -2
       act = new_price  ).

  ENDMETHOD.

  METHOD test_change_90.

    NEW zcl_demo_test_seams( )->change_price(
      EXPORTING
         carrid = '   '
         connid = '0000'
         fldate = '00000000'
         factor = 90
       IMPORTING new_price = DATA(new_price) ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       exp = 90
       act = new_price  ).

  ENDMETHOD.



  METHOD test_change_price.
    TEST-INJECTION authorisation.
      sy-subrc = 4.
    END-TEST-INJECTION.
    invoke_and_assert( -3 ).

    TEST-INJECTION authorisation.
    END-TEST-INJECTION.

    TEST-INJECTION modification.
    END-TEST-INJECTION.
    invoke_and_assert( 90 ).

    TEST-INJECTION modification.
      sy-subrc = 4.
    END-TEST-INJECTION.
    invoke_and_assert( -2 ).

    TEST-INJECTION selection.
      sy-subrc = 4.
    END-TEST-INJECTION.
    TEST-INJECTION modification.
    END-TEST-INJECTION.
    invoke_and_assert( -1 ).
  ENDMETHOD.

  METHOD invoke_and_assert.
    NEW zcl_demo_test_seams( )->change_price(
      EXPORTING
         carrid = '   '
         connid = '0000'
         fldate = '00000000'
         factor = 90
       IMPORTING new_price = DATA(new_price) ).
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       exp = exp
       act = new_price  ).
  ENDMETHOD.


ENDCLASS.
