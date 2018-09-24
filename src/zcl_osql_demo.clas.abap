class ZCL_OSQL_DEMO definition
  public
  final
  create public .

public section.

  methods GET_PRODUCT_INFO
    importing
      !IV_CUSTOMER_ID type CSEQUENCE
    exporting
      !ET_PRODUCT type ANY TABLE .
  methods GET_CUSTOMER_INFO
    importing
      !IV_CUSTOMER_ID type CSEQUENCE
    exporting
      !ET_CUSTOMER_INFO type ANY TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OSQL_DEMO IMPLEMENTATION.


  METHOD GET_CUSTOMER_INFO.

**********************************************************************
* Retrieves the customer details of a given customer id
**********************************************************************

    DATA : lv_exists  TYPE abap_bool VALUE abap_false.

    "generic existence check for sales order for the given customer
    SELECT SINGLE @abap_true FROM SEPM_I_SalesOrder AS so JOIN SEPM_I_BusinessPartner AS customer ON
    so~customeruuid = customer~businesspartneruuid INTO @lv_exists WHERE customer~businesspartner = @iv_customer_id.

    IF lv_exists = abap_true.

      "fetch the customer name and the gross amount for the given customer id
      SELECT customer~companyname AS customer_name , SUM( so~GrossAmountInTransacCurrency ) AS sum_gross_amount
      FROM SEPM_I_SalesOrder AS so JOIN SEPM_I_BusinessPartner AS customer ON
      so~customeruuid = customer~businesspartneruuid
      WHERE customer~businesspartner = @iv_customer_id
      GROUP BY customer~companyname
      INTO TABLE @et_customer_info.

    ENDIF.

  ENDMETHOD.


  METHOD GET_PRODUCT_INFO.

**********************************************************************
* Retrieves the product information of a particular customer
**********************************************************************

    DATA : lv_exists  TYPE abap_bool VALUE abap_false.

    DATA(lv_logon_language) = cl_abap_syst=>get_logon_language( ).
    DATA(mc_default_lang) = cl_abap_syst=>get_language( ).

    "generic existence check : to check whether sales order is not empty
    SELECT SINGLE @abap_true FROM SEPM_I_SalesOrder AS so JOIN SEPM_I_BusinessPartner AS customer ON
    so~customeruuid = customer~businesspartneruuid INTO @lv_exists WHERE customer~businesspartner = @iv_customer_id.

    IF lv_exists = abap_true.

      "fetch the product information of the given customer from the CDS osqlfrwk_product_info
      SELECT * FROM osqlfrwk_product_info( langu = @lv_logon_language,fallback_langu = @mc_default_lang )
      WHERE customer_id = @iv_customer_id
      ORDER BY product_cnt DESCENDING INTO TABLE @et_product.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
