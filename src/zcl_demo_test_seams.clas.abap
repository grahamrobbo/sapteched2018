CLASS zcl_demo_test_seams DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS change_price
      IMPORTING
        !carrid          TYPE sflight-carrid
        !connid          TYPE sflight-connid
        !fldate          TYPE sflight-fldate
        !factor          TYPE i
      EXPORTING
        VALUE(new_price) TYPE sflight-price .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_test_seams IMPLEMENTATION.


  METHOD change_price.
    DATA wa TYPE sflight.

    TEST-SEAM authorisation.
      AUTHORITY-CHECK OBJECT 'S_FLBOOK'
         ID 'ACTVT' FIELD '02'.
    END-TEST-SEAM.

    IF sy-subrc <> 0.
      new_price = - 3.
      RETURN.
    ENDIF.

    TEST-SEAM selection.
      SELECT SINGLE *
             FROM sflight
             INTO wa
             WHERE carrid = carrid AND
                   connid = connid AND
                   fldate = fldate.
    END-TEST-SEAM.

    IF sy-subrc <> 0.
      new_price = - 1.
      RETURN.
    ENDIF.

    wa-price = wa-price * factor / 100.

    TEST-SEAM modification.
      MODIFY sflight FROM wa.
    END-TEST-SEAM.

    IF sy-subrc = 0.
      new_price = wa-price.
    ELSE.
      new_price = - 2.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
