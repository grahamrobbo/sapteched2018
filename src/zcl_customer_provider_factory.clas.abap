CLASS zcl_customer_provider_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_customer_factory_injector.
  PUBLIC SECTION.
    CLASS-METHODS get_customer_provider
      IMPORTING
                !node_key         TYPE snwd_node_key
      RETURNING VALUE(r_provider) TYPE REF TO zif_customer_provider
      RAISING
                cx_abap_invalid_value .

    CLASS-METHODS get_node_key_from_bp_id
      IMPORTING bp_id           TYPE snwd_partner_id
      RETURNING VALUE(node_key) TYPE snwd_node_key
      RAISING   cx_abap_invalid_value.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF provider_type,
        node_key TYPE snwd_node_key,
        instance TYPE REF TO zif_customer_provider,
      END OF provider_type .

    CLASS-DATA providers TYPE TABLE OF provider_type .

    CLASS-METHODS reset_customer_providers.

ENDCLASS.

CLASS zcl_customer_provider_factory IMPLEMENTATION.
  METHOD get_customer_provider.
    TRY.
        DATA(provider) = providers[ node_key = node_key ].
      CATCH cx_sy_itab_line_not_found.
        provider-node_key = node_key.
        provider-instance = NEW zcl_customer_provider( provider-node_key ).
        APPEND provider TO providers.
    ENDTRY.

    r_provider ?= provider-instance.

  ENDMETHOD.

  METHOD reset_customer_providers.
    CLEAR zcl_customer_provider_factory=>providers.
  ENDMETHOD.

  METHOD get_node_key_from_bp_id.
    DATA: lv_bp_id TYPE snwd_partner_id.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = bp_id
      IMPORTING
        output = lv_bp_id.

    LOOP AT providers REFERENCE INTO DATA(provider).
      IF provider->instance->get_bp_id( ) = lv_bp_id.
        node_key = provider->instance->get_node_key( ).
        RETURN.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE node_key
      FROM snwd_bpa
      INTO @node_key
      WHERE bp_id = @lv_bp_id.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_abap_invalid_value
        EXPORTING
          textid = cx_abap_invalid_value=>cx_root.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
