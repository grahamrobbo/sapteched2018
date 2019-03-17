CLASS zcl_customer_display DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_customer_display IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TRY.
        SELECT bp_id
          FROM snwd_bpa
          INTO @DATA(bp_id)
            UP TO 10 ROWS.

          DATA(customerV1) = zcl_customer_b4=>zif_customer~get_using_bp_id( bp_id ).
          DATA(customerV2) = zcl_customer=>zif_customer~get_using_bp_id( bp_id ).

          out->write_text( |V1 Customer { customerV1->get_bp_id( ) } { customerV1->get_company_name( ) } { customerV1->get_address( ) }| ).
          out->write_text( |V2 Customer { customerV2->get_bp_id( ) } { customerV2->get_company_name( ) } { customerV2->get_address( ) }| ).

        ENDSELECT.

      CATCH cx_root INTO DATA(cx).
        out->write( cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
