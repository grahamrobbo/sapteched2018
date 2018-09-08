INTERFACE zif_customer
  PUBLIC .

  TYPES:
    BEGIN OF instance_type,
      node_key TYPE snwd_node_key,
      instance TYPE REF TO zif_customer,
    END OF instance_type .
  TYPES:
    instance_ttype TYPE TABLE OF instance_type .

  CLASS-DATA instances TYPE instance_ttype .

  CLASS-METHODS get
    IMPORTING
      !node_key       TYPE snwd_node_key
    RETURNING
      VALUE(instance) TYPE REF TO zif_customer
    RAISING
      cx_abap_invalid_value .
  CLASS-METHODS get_using_bp_id
    IMPORTING
      !bp_id          TYPE snwd_partner_id
    RETURNING
      VALUE(instance) TYPE REF TO zif_customer
    RAISING
      cx_abap_invalid_value .
  METHODS get_node_key
    RETURNING
      VALUE(node_key) TYPE snwd_node_key
    RAISING
      cx_abap_invalid_value .
  METHODS get_bp_id
    RETURNING
      VALUE(bp_id) TYPE snwd_partner_id
    RAISING
      cx_abap_invalid_value .
  METHODS get_company_name
    RETURNING
      VALUE(company_name) TYPE snwd_company_name
    RAISING
      cx_abap_invalid_value .
  METHODS get_street
    RETURNING
      VALUE(street) TYPE snwd_street
    RAISING
      cx_abap_invalid_value .
  METHODS get_city
    RETURNING
      VALUE(city) TYPE snwd_city
    RAISING
      cx_abap_invalid_value .
  METHODS get_postal_code
    RETURNING
      VALUE(postal_code) TYPE snwd_postal_code
    RAISING
      cx_abap_invalid_value .
  METHODS get_country
    RETURNING
      VALUE(country) TYPE snwd_country
    RAISING
      cx_abap_invalid_value .
  METHODS get_country_text
    RETURNING
      VALUE(country_text) TYPE landx50
    RAISING
      cx_abap_invalid_value .
  METHODS get_address
    RETURNING
      VALUE(address) TYPE string
    RAISING
      cx_abap_invalid_value .
ENDINTERFACE.
