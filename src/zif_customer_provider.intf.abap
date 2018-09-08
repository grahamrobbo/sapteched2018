INTERFACE zif_customer_provider
  PUBLIC .
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
ENDINTERFACE.
