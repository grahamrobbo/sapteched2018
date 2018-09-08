CLASS zcl_customer DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_customer .

    ALIASES get
      FOR zif_customer~get .
    ALIASES get_bp_id
      FOR zif_customer~get_bp_id .
    ALIASES get_city
      FOR zif_customer~get_city .
    ALIASES get_company_name
      FOR zif_customer~get_company_name .
    ALIASES get_country
      FOR zif_customer~get_country .
    ALIASES get_country_text
      FOR zif_customer~get_country_text .
    ALIASES get_node_key
      FOR zif_customer~get_node_key .
    ALIASES get_postal_code
      FOR zif_customer~get_postal_code .
    ALIASES get_street
      FOR zif_customer~get_street .
    ALIASES get_using_bp_id
      FOR zif_customer~get_using_bp_id .

    METHODS constructor
      IMPORTING
        !node_key TYPE snwd_node_key
      RAISING
        cx_abap_invalid_value .
  PROTECTED SECTION.

    DATA: customer_provider TYPE REF TO zif_customer_provider.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF instance_type,
        node_key TYPE snwd_node_key,
        instance TYPE REF TO zif_customer,
      END OF instance_type .
    TYPES:
      instance_ttype TYPE TABLE OF instance_type .

    CLASS-DATA instances TYPE instance_ttype .
ENDCLASS.

CLASS zcl_customer IMPLEMENTATION.

  METHOD constructor.
    customer_provider ?= zcl_customer_provider_factory=>get_customer_provider( node_key ).
  ENDMETHOD.

  METHOD zif_customer~get_bp_id.
    bp_id = me->customer_provider->get_bp_id( ).
  ENDMETHOD.

  METHOD zif_customer~get_city.
    city = me->customer_provider->get_city( ).
  ENDMETHOD.

  METHOD zif_customer~get_company_name.
    company_name = me->customer_provider->get_company_name( ).
  ENDMETHOD.

  METHOD zif_customer~get_country.
    country = me->customer_provider->get_country( ).
  ENDMETHOD.

  METHOD zif_customer~get_country_text.
    country_text = me->customer_provider->get_country_text( ).
  ENDMETHOD.

  METHOD zif_customer~get_node_key.
    node_key = me->customer_provider->get_node_key( ).
  ENDMETHOD.

  METHOD zif_customer~get_postal_code.
    postal_code = me->customer_provider->get_postal_code( ).
  ENDMETHOD.

  METHOD zif_customer~get_street.
    street = me->customer_provider->get_street( ).
  ENDMETHOD.

  METHOD zif_customer~get_address.
    address = me->customer_provider->get_address( ).
  ENDMETHOD.

  METHOD zif_customer~get.

    TRY.
        DATA(inst) = instances[ node_key = node_key ].
      CATCH cx_sy_itab_line_not_found.
        inst-node_key = node_key.
        inst-instance = NEW zcl_customer( inst-node_key ).
        APPEND inst TO instances.
    ENDTRY.

    instance ?= inst-instance.

  ENDMETHOD.

  METHOD zif_customer~get_using_bp_id.
    instance = zif_customer~get( zcl_customer_provider_factory=>get_node_key_from_bp_id( bp_id ) ).
  ENDMETHOD.

ENDCLASS.
