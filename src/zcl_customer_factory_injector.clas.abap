***********************************************************************************************************
*  During tests, the object factory shall return test doubles to the code under test
*  ▪ As a test developer, you need a secure technique to inject test doubles into the object factory class
*  ▪ With an injector class, we provide a secure way to inject test doubles in the object factory class
*  ▪ Injector class…
*    – must only be available for tests. It is therefore declared as test class
*    – must be global friend of the factory class to modify object factory class internals
***********************************************************************************************************

CLASS zcl_customer_factory_injector DEFINITION FOR TESTING "secure access only in test
  PUBLIC
  FINAL
  CREATE PRIVATE . "static class
  PUBLIC SECTION.
    CLASS-METHODS inject_customer_provider
      IMPORTING i_customer_provider TYPE REF TO zif_customer_provider
      RAISING   cx_abap_invalid_value.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_customer_factory_injector IMPLEMENTATION.
  METHOD inject_customer_provider.
    DELETE zcl_customer_provider_factory=>providers WHERE node_key = i_customer_provider->get_node_key( ).
    APPEND INITIAL LINE TO zcl_customer_provider_factory=>providers REFERENCE INTO DATA(provider).
    provider->node_key = i_customer_provider->get_node_key( ).
    provider->instance = i_customer_provider.
  ENDMETHOD.

ENDCLASS.
