CLASS zcl_customer_b4 DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_customer .

    ALIASES get
      FOR zif_customer~get .

    METHODS constructor
      IMPORTING
        !node_key TYPE snwd_node_key
      RAISING
        cx_abap_invalid_value .
  PROTECTED SECTION.

    CLASS-DATA countries TYPE TABLE OF t005t.

    TYPES: BEGIN OF customer_type,
             node_key     TYPE snwd_node_key,
             bp_id        TYPE snwd_partner_id,
             company_name TYPE snwd_company_name,
             street       TYPE snwd_street,
             city         TYPE snwd_city,
             postal_code  TYPE snwd_postal_code,
             country      TYPE snwd_country,
             country_text TYPE zdemo_country_text,
           END OF customer_type.

    DATA: customer_data TYPE customer_type.

    METHODS load_customer_data
      IMPORTING
        !node_key TYPE snwd_node_key
      RAISING
        cx_abap_invalid_value .
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_customer_b4 IMPLEMENTATION.

  METHOD zif_customer~get.

    TRY.
        DATA(inst) = zif_customer~instances[ node_key = node_key ].
      CATCH cx_sy_itab_line_not_found.
        inst-node_key = node_key.
        inst-instance = NEW zcl_customer_b4(  inst-node_key ).
        APPEND inst TO zif_customer~instances.
    ENDTRY.

    instance ?= inst-instance.

  ENDMETHOD.

  METHOD constructor.
    load_customer_data( node_key ).
  ENDMETHOD.

  METHOD load_customer_data.

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

  METHOD zif_customer~get_node_key.
    node_key = me->customer_data-node_key.
  ENDMETHOD.

  METHOD zif_customer~get_bp_id.
    bp_id = me->customer_data-bp_id.
  ENDMETHOD.

  METHOD zif_customer~get_city.
    city = me->customer_data-city.
  ENDMETHOD.

  METHOD zif_customer~get_company_name.
    company_name = me->customer_data-company_name.
  ENDMETHOD.

  METHOD zif_customer~get_country.
    country = me->customer_data-country.
  ENDMETHOD.

  METHOD zif_customer~get_postal_code.
    postal_code = me->customer_data-postal_code.
  ENDMETHOD.

  METHOD zif_customer~get_street.
    street = me->customer_data-street.
  ENDMETHOD.

  METHOD zif_customer~get_address.
    address = |{ zif_customer~get_street( ) }, { zif_customer~get_city( ) } { zif_customer~get_postal_code( ) }, { zif_customer~get_country_text( ) }|.
  ENDMETHOD.

  METHOD zif_customer~get_country_text.
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

  METHOD zif_customer~get_using_bp_id.

    DATA: lv_bp_id TYPE snwd_partner_id.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = bp_id
      IMPORTING
        output = lv_bp_id.

    SELECT SINGLE node_key
      FROM snwd_bpa
      INTO @DATA(node_key)
      WHERE bp_id = @lv_bp_id.
    IF sy-subrc = 0.
      instance = zif_customer~get( node_key ).
    ELSE.
      RAISE EXCEPTION TYPE cx_abap_invalid_value
        EXPORTING
          textid = cx_abap_invalid_value=>cx_root.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
