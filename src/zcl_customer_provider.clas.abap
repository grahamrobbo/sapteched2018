CLASS zcl_customer_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_customer_provider_factory .

  PUBLIC SECTION.
    INTERFACES zif_customer_provider .

    METHODS constructor
      IMPORTING
        !node_key TYPE snwd_node_key
      RAISING
        cx_abap_invalid_value .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA countries TYPE TABLE OF t005t.

    DATA customer_data TYPE zif_customer_provider=>customer_type.

    METHODS load_data
      IMPORTING
        !node_key TYPE snwd_node_key
      RAISING
        cx_abap_invalid_value .

ENDCLASS.

CLASS zcl_customer_provider IMPLEMENTATION.

  METHOD constructor.
    me->load_data( node_key ).
  ENDMETHOD.

  METHOD load_data.
    SELECT bp~node_key, bp~bp_id, bp~company_name,
      ad~street, ad~city, ad~postal_code, ad~country
      UP TO 1 ROWS
      FROM snwd_bpa AS bp
        INNER JOIN snwd_ad AS ad
        ON bp~address_guid = ad~node_key
      INTO CORRESPONDING FIELDS OF @me->customer_data
      WHERE bp~node_key = @node_key.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE cx_abap_invalid_value
          EXPORTING
            textid = cx_abap_invalid_value=>cx_root.
      ENDIF.
    ENDSELECT.

  ENDMETHOD.

  METHOD zif_customer_provider~get_node_key.
    node_key = me->customer_data-node_key.
  ENDMETHOD.

  METHOD zif_customer_provider~get_bp_id.
    bp_id = me->customer_data-bp_id.
  ENDMETHOD.

  METHOD zif_customer_provider~get_company_name.
    company_name = me->customer_data-company_name.
  ENDMETHOD.

  METHOD zif_customer_provider~get_street.
    street = me->customer_data-street.
  ENDMETHOD.

  METHOD zif_customer_provider~get_city.
    city = me->customer_data-city.
  ENDMETHOD.

  METHOD zif_customer_provider~get_postal_code.
    postal_code = me->customer_data-postal_code.
  ENDMETHOD.

  METHOD zif_customer_provider~get_country.
    country = me->customer_data-country.
  ENDMETHOD.

  METHOD zif_customer_provider~get_country_text.
    TRY.
        country_text = countries[ land1 = me->customer_data-country ]-landx50.
      CATCH cx_sy_itab_line_not_found.
        SELECT land1 landx50
          FROM t005t
          APPENDING CORRESPONDING FIELDS OF TABLE countries
          WHERE spras = sy-langu
          AND land1 = me->customer_data-country.
        IF sy-subrc = 0.
          country_text = countries[ land1 = me->customer_data-country ]-landx50.
        ELSE.
          RAISE EXCEPTION TYPE cx_abap_invalid_value
            EXPORTING
              textid = cx_abap_invalid_value=>cx_root.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
