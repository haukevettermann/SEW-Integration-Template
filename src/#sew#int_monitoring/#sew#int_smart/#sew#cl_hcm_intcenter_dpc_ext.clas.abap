class /SEW/CL_HCM_INTCENTER_DPC_EXT definition
  public
  inheriting from /SEW/CL_HCM_INTCENTER_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_END
    redefinition .
  methods /IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_START
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.

  methods FIELDCHANGESET_GET_ENTITY
    redefinition .
  methods FIELDCHANGESET_GET_ENTITYSET
    redefinition .
  methods FIELDCHANGESET_UPDATE_ENTITY
    redefinition .
  methods INFOTYPECHANGESE_GET_ENTITY
    redefinition .
  methods INFOTYPECHANGESE_GET_ENTITYSET
    redefinition .
  methods INTEGRATIONRUNSE_GET_ENTITY
    redefinition .
  methods INTEGRATIONRUNSE_GET_ENTITYSET
    redefinition .
  methods MESSAGESET_GET_ENTITY
    redefinition .
  methods MESSAGESET_GET_ENTITYSET
    redefinition .
  methods ORGCHANGESET_GET_ENTITY
    redefinition .
  methods ORGCHANGESET_GET_ENTITYSET
    redefinition .
private section.

  types:
    BEGIN OF ty_full_key,
        int_run  TYPE guid_32,
        sap_id   TYPE hrobjid,
        pernr    TYPE pernr_d,
        cloud_id TYPE /sew/dd_objectid,
        aend_id  TYPE guid_32,
        msg_guid TYPE guid_32,
        field_id TYPE guid_32,
      END OF ty_full_key .

  methods ENHANCE_ENTITY_FIELDCHANGE
    changing
      !CT_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TT_FIELDCHANGE optional
      !CS_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TS_FIELDCHANGE optional .
  methods ENHANCE_ENTITY_IT_AEND
    changing
      !CT_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TT_INFOTYPECHANGE optional
      !CS_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TS_INFOTYPECHANGE optional .
  methods ENHANCE_ENTITY_MESSAGE
    changing
      !CT_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TT_MESSAGE optional
      !CS_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TS_MESSAGE optional .
  methods ENHANCE_ENTITY_OM_AEND
    changing
      !CT_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TT_ORGCHANGE optional
      !CS_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TS_ORGCHANGE optional .
  methods CREATE_IMAGE_P
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJID
    exporting
      !EV_XSTRING type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING .
  methods CREATE_IMAGE_M
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJID
    exporting
      !EV_XSTRING type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING .
  methods CREATE_IMAGE_O
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJID
    exporting
      !EV_XSTRING type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING .
  methods CREATE_IMAGE_I
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJID
    exporting
      !EV_XSTRING type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING .
  methods CREATE_IMAGE_F
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJID
    exporting
      !EV_XSTRING type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING .
  methods ENHANCE_ENTITY_INTRUN
    changing
      !CS_ENTITY type /SEW/CL_HCM_INTCENTER_MPC=>TS_INTEGRATIONRUN optional .
ENDCLASS.



CLASS /SEW/CL_HCM_INTCENTER_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
    cv_defer_mode = abap_true.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.
    DATA:
      ls_entity_changes TYPE /sew/cl_hcm_intcenter_mpc=>ts_fieldchange,
      lo_data           TYPE REF TO data.

    LOOP AT it_changeset_request INTO DATA(ls_changeset_request).
      CASE ls_changeset_request-operation_type.
        WHEN /iwbep/if_mgw_appl_types=>gcs_operation_type-patch_entity.
          DATA(lo_patch_context) = CAST /iwbep/if_mgw_req_entity_p( ls_changeset_request-request_context ).
          DATA(lo_update_context) = CAST /iwbep/if_mgw_req_entity_u( ls_changeset_request-request_context ).
          DATA(lo_entity_context) = CAST /iwbep/if_mgw_req_entity( ls_changeset_request-request_context ).

          CASE lo_patch_context->get_entity_type_name( ).
            WHEN 'FieldChange'.
              me->fieldchangeset_get_entity(
                EXPORTING
                  iv_entity_name          = lo_entity_context->get_entity_type_name( )
                  iv_entity_set_name      = lo_entity_context->get_entity_set_name( )
                  iv_source_name          = VALUE #( )
                  it_key_tab              = VALUE #( )
                  io_tech_request_context = lo_entity_context
                  it_navigation_path      = VALUE #( )
                IMPORTING
                  er_entity               = DATA(ls_entity)
              ).

              ls_changeset_request-entry_provider->read_entry_data( IMPORTING es_data = ls_entity_changes ).
              DATA(lt_components) = lo_patch_context->get_components( ).
              LOOP AT lt_components INTO DATA(ls_component).
                ASSIGN COMPONENT ls_component-property OF STRUCTURE ls_entity TO FIELD-SYMBOL(<lv_value_old>).
                ASSIGN COMPONENT ls_component-property OF STRUCTURE ls_entity_changes TO FIELD-SYMBOL(<lv_value_new>).
                MOVE <lv_value_new> TO <lv_value_old>.
              ENDLOOP.

              CREATE DATA lo_data LIKE ls_entity.
              ASSIGN lo_data->* TO FIELD-SYMBOL(<ls_entity>).
              MOVE-CORRESPONDING ls_entity TO <ls_entity>.

              DATA(lo_data_provider) = NEW /iwbep/cl_mgw_entry_patch_prv( ir_data = lo_data it_components = lt_components ).

              me->fieldchangeset_update_entity(
                EXPORTING
                  iv_entity_name          = lo_entity_context->get_entity_type_name( )
                  iv_entity_set_name      = lo_entity_context->get_entity_set_name( )
                  iv_source_name          = VALUE #( )
                  it_key_tab              = VALUE #( )
                  io_tech_request_context = lo_update_context
                  it_navigation_path      = VALUE #( )
                  io_data_provider        = lo_data_provider
                IMPORTING
                  er_entity               = <ls_entity>
              ).

              APPEND VALUE #( operation_no = ls_changeset_request-operation_no entity_data = lo_data ) TO ct_changeset_response.
            WHEN OTHERS.
              super->/iwbep/if_mgw_appl_srv_runtime~changeset_process( EXPORTING it_changeset_request = it_changeset_request
                CHANGING ct_changeset_response = ct_changeset_response ).
          ENDCASE.
        WHEN OTHERS.
          super->/iwbep/if_mgw_appl_srv_runtime~changeset_process( EXPORTING it_changeset_request = it_changeset_request
            CHANGING ct_changeset_response = ct_changeset_response ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
    DATA:
      ls_int_run_entity TYPE /sew/cl_hcm_intcenter_mpc=>ts_integrationrun,
      cloud_id          TYPE /sew/dd_value.

    CASE iv_action_name.
      WHEN 'markIntegrationRunAsDone' OR 'markIntegrationRunAsProcessed' OR 'startIntegrationRunImmediately'.
* schlüsseldaten lesen
        /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext_i = io_tech_request_context
          IMPORTING et_key_tab = DATA(lt_key_tab) ).
        /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab
          CHANGING cs_any = ls_int_run_entity ).

        DATA(lv_new_status) = '01'. " Zurücksetzen auf initial
        IF iv_action_name = 'startIntegrationRunImmediately'.
          lv_new_status = '08'. " Auf "Running" umstellen
        ELSEIF iv_action_name = 'markIntegrationRunAsDone'.
          lv_new_status = '07'. " Auf "Done" umstellen
        ENDIF.

* daten lesen
        SELECT SINGLE int_run, sap_id, cloud_id, otype, stream_direction, MIN( timestamp ) AS timestamp, SUM( counter_msg ) AS counter_msg,
          SUM( counter_msg_e ) AS counter_msg_e FROM /sew/int_center_le INTO CORRESPONDING FIELDS OF @ls_int_run_entity WHERE int_run = @ls_int_run_entity-int_run
          AND sap_id = @ls_int_run_entity-sap_id AND cloud_id = @ls_int_run_entity-cloud_id GROUP BY int_run, sap_id, cloud_id, otype, stream_direction.

        IF iv_action_name = 'startIntegrationRunImmediately'.
          sy-langu = 'E'.
* delete old messages
          DATA:
            aend_id_range TYPE rsdsselopt_t,
            aend_id       LIKE LINE OF aend_id_range.

          IF ls_int_run_entity-otype = 'P'.
            SELECT * FROM /sew/int_it_aend INTO TABLE @DATA(it_aend_tab) WHERE int_run = @ls_int_run_entity-int_run AND cloud_pernr = @ls_int_run_entity-cloud_id.
*                                                                    AND pernr = ls_int_run_entity-sap_id
            LOOP AT it_aend_tab INTO DATA(it_aend).
              CLEAR: aend_id.
              aend_id-low = it_aend-aend_id.
              aend_id-option = 'EQ'.
              aend_id-sign = 'I'.
              APPEND aend_id TO aend_id_range.
            ENDLOOP.
          ELSE.
            SELECT * FROM /sew/int_om_aend INTO TABLE @DATA(om_aend_tab) WHERE int_run = @ls_int_run_entity-int_run AND cloud_id = @ls_int_run_entity-cloud_id.
*                                                                    AND pernr = ls_int_run_entity-sap_id
            LOOP AT om_aend_tab INTO DATA(om_aend).
              CLEAR: aend_id.
              DATA(ext_type) = om_aend-ext_type.
              aend_id-low = om_aend-aend_id.
              aend_id-option = 'EQ'.
              aend_id-sign = 'I'.
              APPEND aend_id TO aend_id_range.
            ENDLOOP.
          ENDIF.
          IF aend_id_range IS NOT INITIAL.
            SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
            SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
            SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range.

            DELETE /sew/int_msg_l FROM TABLE msg_l.
            DELETE /sew/int_msg_p FROM TABLE msg_p.
            DELETE /sew/int_msg_f FROM TABLE msg_f.
          ENDIF.
          IF ls_int_run_entity-otype = 'P'.
            UPDATE /sew/int_it_aend SET status = @lv_new_status, aedtm = @sy-datlo, uname = @sy-uname WHERE int_run = @ls_int_run_entity-int_run
              AND pernr = @ls_int_run_entity-sap_id AND cloud_pernr = @ls_int_run_entity-cloud_id.
          ELSEIF ls_int_run_entity-otype = 'O'.
            UPDATE /sew/int_om_aend SET status = @lv_new_status, aedtm = @sy-datlo, uname = @sy-uname WHERE int_run = @ls_int_run_entity-int_run
              AND sap_id = @ls_int_run_entity-sap_id AND cloud_id = @ls_int_run_entity-cloud_id.

            "check if Department or Department Tree

          ELSE.
            "NOOP
          ENDIF.
          READ TABLE it_aend_tab ASSIGNING FIELD-SYMBOL(<it_aend_line>) INDEX 1.
          IF ls_int_run_entity-otype = 'P'.
            cloud_id = <it_aend_line>-cloud_id.
          ELSEIF ls_int_run_entity-otype = 'O'.
            cloud_id = ls_int_run_entity-cloud_id.
          ENDIF.
          /sew/cl_int_utility=>start_int_run( int_run = ls_int_run_entity-int_run cloud_id = CONV #( cloud_id )
                                                sap_id = CONV #( ls_int_run_entity-sap_id ) type = ls_int_run_entity-otype
                                                 ext_type = CONV #( ext_type ) ).
        ELSE.
          IF ls_int_run_entity-otype = 'P'.
            UPDATE /sew/int_it_aend SET status = @lv_new_status, aedtm = @sy-datlo, uname = @sy-uname WHERE int_run = @ls_int_run_entity-int_run
              AND pernr = @ls_int_run_entity-sap_id AND cloud_pernr = @ls_int_run_entity-cloud_id AND status = '03'.
          ELSEIF ls_int_run_entity-otype = 'O'.
            UPDATE /sew/int_om_aend SET status = @lv_new_status, aedtm = @sy-datlo, uname = @sy-uname WHERE int_run = @ls_int_run_entity-int_run
              AND sap_id = @ls_int_run_entity-sap_id AND cloud_id = @ls_int_run_entity-cloud_id AND status = '03'.
          ELSE.
            "NOOP
          ENDIF.
        ENDIF.

        enhance_entity_intrun( CHANGING cs_entity = ls_int_run_entity ).

        copy_data_to_ref(  EXPORTING is_data = ls_int_run_entity CHANGING cr_data = er_data ).

      WHEN OTHERS.
        CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~execute_action
          EXPORTING
            iv_action_name          = iv_action_name
            it_parameter            = it_parameter
            io_tech_request_context = io_tech_request_context
          IMPORTING
            er_data                 = er_data.
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA:
      ls_image    TYPE /sew/cl_hcm_intcenter_mpc=>ts_image,
      ls_stream   TYPE ty_s_media_resource,
      lv_filename TYPE string.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context
      IMPORTING et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ).

    IF lv_entityset = 'IMAGESET'.
      /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab
        CHANGING cs_any = ls_image ).
      CASE ls_image-otype.
        WHEN 'P'.
          create_image_p( EXPORTING iv_otype = ls_image-otype iv_objid = ls_image-objid IMPORTING ev_xstring = ls_stream-value
            ev_filename = lv_filename ev_mimetype = ls_stream-mime_type ).
        WHEN 'O'.
          create_image_o( EXPORTING iv_otype = ls_image-otype iv_objid = ls_image-objid IMPORTING ev_xstring = ls_stream-value
            ev_filename = lv_filename ev_mimetype = ls_stream-mime_type ).
        WHEN 'I'.
          create_image_i( EXPORTING iv_otype = ls_image-otype iv_objid = ls_image-objid IMPORTING ev_xstring = ls_stream-value
            ev_filename = lv_filename ev_mimetype = ls_stream-mime_type ).
        WHEN 'M'.
          create_image_m( EXPORTING iv_otype = ls_image-otype iv_objid = ls_image-objid IMPORTING ev_xstring = ls_stream-value
            ev_filename = lv_filename ev_mimetype = ls_stream-mime_type ).
        WHEN 'F'.
          create_image_f( EXPORTING iv_otype = ls_image-otype iv_objid = ls_image-objid IMPORTING ev_xstring = ls_stream-value
            ev_filename = lv_filename ev_mimetype = ls_stream-mime_type ).
      ENDCASE.
    ENDIF.


    IF lv_filename IS NOT INITIAL.
      DATA(ls_lheader) = VALUE ihttpnvp( name = 'Content-Disposition' value = |inline; filename="{ lv_filename }";| ).
      set_header( is_header = ls_lheader ).
    ENDIF.

    IF ls_stream IS NOT INITIAL.
      copy_data_to_ref( EXPORTING is_data = ls_stream CHANGING cr_data = er_stream ).
    ENDIF.
  ENDMETHOD.


  method /IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_END.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_END
*    .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method /IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_START.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_SOST_SRV_RUNTIME~OPERATION_START
**  EXPORTING
**    iv_is_first_request =
**    iv_is_last_request  =
*    .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  METHOD CREATE_IMAGE_F.
    DATA(lv_imagebase64) =
      |iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAAAA8AXHiAAAABmJLR0QA/wD/AP+gvaeTAA| &&
      |AACXBIWXMAAA7DAAAOwwHHb6hkAAAAB3RJTUUH5QIPCg0VfRp81wAACcxJREFUeNrtnWuM| &&
      |XGUZx3+zs73SSqBFpSBbekX6yRiNhYptthcIGhNiDMUCrahR460qVIv1i+0utdUEE6N+MQ| &&
      |EtyOUTCbvWNtRbpVL8ZKL1QmOQRkHBS9jdabsz44fnOe7s7pydmd09Z8575v9Lmm52pj3d| &&
      |M78+7/O+53neF4QQQgghhBBCCCGEEEIIMXsUUrzWHKCoW942ysDFtC7WndJ1uoCtwDv8a5| &&
      |EuFeA0MOhf50qszcDHFbXaFq2+CxzNm1iRXEUfEkX6dOX2YkIWCyGxhMQSEkuIMGaFzVDV| &&
      |RzJtChIrnleBP8uRllkJXCGx4jkOfAUYlitNsxD4GrBdYsUzAvxNYrUsVknJe0B5gvIrzQ| &&
      |qFxBISS6TNXIklZpu3AJ8CevPww3Tr88wE1wK7gQ8DvwZGgZ8pYomZsBz4HHAXcAnwHmCf| &&
      |/66IJabFVR6p7gYu9e8VgY3+dRX4uSKWaIWrgS8BH6uRihq5bgI+ACxRxBIRBaZ+oH6tD3| &&
      |+7gHl1Xq94rjUI/EsRSwCsAN5L/EPhHuCzNTlVoY5UvwIOkGLzgyKW/Q+/HphPso8yqsBZ| &&
      |4OUW/kwPcC+wDfg+8O0JEWeZ51Q76wx/kVTPAn3AsVClClWs5cBhYE3C1xkB9gMPt7BksM| &&
      |eHt7nAp33Z4FvYQ/WrgPs8p5ofI9Upl+poyFKFKlYRuNI/qCQZwioHmmG150w7GGtvuwL4| &&
      |gn896DO/qXKqZ0Mf/vKQvFdrkuR2cwlWC/Uh/7o2gV/iQ9+twDpgcQOpfpIHqTQrnB3OY1| &&
      |Wv54A3TJC9ALwRWBozUYpmf30uVTkvN0WzwpkzCjwOPAD8PmaZoSsm6j7nUv04T1KFGrGq| &&
      |WG38KynkWKUW5HrCBdoDXNdgmI4S9dzkVHkQ6y/YivXChK9TBv7UwvtLwI9cki9PIdfE2V| &&
      |+ZHBKiWCPYAmJW861HParurSNXpWb4y61UyrGSy7keAw4CZ2pyrok51Wieb4Jmhclw0RP6| &&
      |gudca3z2dyCPibrESn/IftSHv1uBH3SKVBIrnZzrCPAM1i9Z7pQfXGIlTxl4qdN+aCXvQm| &&
      |IJiSUklsgQ88jJ/hUSKztcgxUBbiQHe+FrVpgdqb6I1cE/j619/ZKAlycUsdrPm7FiwF1Y| &&
      |HXwvtvncBkUsMV2WYSXN9wCLar6/yX8vYK32we3NqojV3uFvD/AJJpcsFz1ivQ+rStVQKP| &&
      |4vxlRcjTVa7JoQqSLKwG+wcuX/SiwBYw2rl8W8/ibGdpap11xRxZor9mM1W0FuUR5ijjUX| &&
      |WEv9NqrZ5kVaK4Fe7bO7m7Fj3L4HvFbz+pWeU30kJlJVgZNYec0xJe/p0uM3fkXC1xkBvo| &&
      |GVGzfDWqwhdTuwAGtYBfgO8G9sY7XPe6K+OGb4izp2jhN4JUSIYs0BVtG4YWGmvD7FcFYv| &&
      |Ut0H3O5SRdFpt/97nwQ+OsXwF0m1H6vZCv6EjtAbVrNwnUt9aLuDya3zS7ENQHqBt8VIBW| &&
      |PVpYN5STSVvM9OZPst9Y9qKQCX+9JBXBf0yZpEPTeEGrGy9KC27EMdWFva9XX+fV0xfy5q| &&
      |rjiGGlbbTgX4J/D3FJL3kSbfW3K5uoD7Pedq1LB62iPVIDk89SxEsV4Evsr4DTiSEvh3Lb| &&
      |y/BDzC+G5oGuRUA3nND0IUaxh7fpbGcNhq6/uoy1XxWeK6On9fJNVRckzIs8KsDh8XsH0c| &&
      |qp5zvdX/E5R9+IukynXDqmaFyeVnTwBfB17wSPW851QDeZcq5IgVAiWsp7AI3IJtOTlAhx| &&
      |xPLLGSZZSxhtVzdNCZ1xIrec5jWy91FMqxhMQSEktILJEh1LAqZp0e4DNYiU3wcmlWmA1W| &&
      |YWfw3I3tr1oBThDw8oQiVvtZidXJ3+FD4U1YhcSmkCOXIlb7h797sTN4omqNIuOP7f0pAe| &&
      |4Dr4jVPpZjrfT3MLkEqAi8G9u7NMiGVUWsZCgydUXoKqxh9U7qN7eWsU1BBlDDqnBWA+/H| &&
      |NvuoxwqXakfM8kIZqzfbjz1j1AmrKTHHE965CV+nij04fq3FRHyvLxk8DDwI/KPm9WuwAs| &&
      |Ad1K+ArWCn1h/w3CrYWWF3oBHhQR9OkmQY6Ad+2MKSwf3YHldd2GYfZeCbwH88Ud/rOVWx| &&
      |gVRBLzWEnGMt8w8q6YbVRU2+9zqf3W2vSS8uwxY8AZ7CmlXvJL5jJ5LqGdSwKrB+wbuADz| &&
      |J+P4kC1sz6SWArVv++oIFUJ8hJzZbEmjkj2GFML2H7N0w8YXWp/yJm+ItOrT9Bjs4tDHVW| &&
      |mKUV6VFs45BDwB9biDgVbA+sQyHP/vIUscrAy0w+fzmJSNRsw+oFl6sLezyzhsYNq8/55O| &&
      |BpdMJqJvgr6TSsVrEznluZRR5x8ff47DXugPHTLtUAOmE1MwwDv8hwvnWEsZ7CiZErGv4i| &&
      |qXLbBqaV99nnAnZO4WHsTOlqHameRiesimkQnQ1d8JxrFdaw2tcJUkmsZBnCVu0r2Ga3D3| &&
      |WKVBIrnZzrIWz/q3PohFUxi4xiWy91FErehcQSEktILJERCljV6VyJJWbzc9iAbdS2k+Qf| &&
      |V2lW2CGR6kZgH7AZeCe29vUI9vhKEUtMixuxNrAtLtlabLV+O1bfL7FEy5Fqg0vVO+G1Nd| &&
      |hD7B2h5lwSq333/QYf/rYwubkiSuRXSixRK8WiBq+vx2rKtsZ8BsPA49izxtclligA27CG| &&
      |1HUx71nvkWprzOsXgcd8hnhGs8L0mIcdhDQ/hWudxcqgm6Hos7p9wNuxCtKD2MlgkXQ3eE| &&
      |61OebviGq5+rFaLi03pEiP3/ikG1ZLfp0jTUrV69Ksd4lu89f6PPK8qyZS1auHH/JI9UDo| &&
      |UoUqVjfWqr6C5BtWFzc5/G3BuqA31Hx/AdZrWMAaVnf6MFmPEc+pDudBqlDFgmw1dV6OHS| &&
      |6+Pub+3g5sxI7yrceoS3UQ+EOepr1iZryKtX7Va44o+D1eFhNdz2Mr7H15kirkiJW1LRSj| &&
      |0+fBzs1p5r4OYQc59WONrkis9lLBthZ6JWHBhjyBb3ZorpXrZqZ+HFNyqQ7lUapQxToL7G| &&
      |b8BhxJ5XEvtPj+Uy5XweXqbpBTnSGnhCjWBayTOKvUyrVtwj0+70sKucup8pJjZZ1T2LZE| &&
      |1Mg1jB1I3p93qSRWssNorVybXKqgH9NIrGzJ1Y/tNfEUrW0yIrHElHKdxPZsKHXSD64F0n| &&
      |QoddoPLLGExBISayY5icjBfcta8r4AqwIYlitNs5B0ih6DFmszdiqWaI2VEmtqlvgvEThZ| &&
      |E6ugj0TJuxASS0gsIbGEkFgip7PCCh20HXXGKJPyQVDdKUp1HDvCVlEyfaKDoVKTK811oz| &&
      |nUPwtZpBe1Luo2CCGEEEIIIYQQQgghhBBCCCGS5H9ghwrjaaepngAAAABJRU5ErkJggg==|.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = lv_imagebase64
      IMPORTING
        output = ev_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_filename = |image_{ iv_otype }_{ iv_objid }.png|.
      ev_mimetype = |image/png|.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_IMAGE_I.
    DATA(lv_imagebase64) =
      |iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAAAA8AXHiAAAABmJLR0QA/wD/AP+gvaeTAA| &&
      |AACXBIWXMAAA7DAAAOwwHHb6hkAAAAB3RJTUUH5QIPByUikCLkIQAAFY9JREFUeNrtnXuU| &&
      |nHV5xz8ze5lsdpMsm0jIDSy5QAgJF4kBkhASbi2IWLBgyym02ipIa3sqtRd6Wj2A2tb2tN| &&
      |jjBYXSY4soFESrUgVUCAEFDQihBEJCiBtyIffNXmdn+8fzfc9MJu9t3tmdncvve857Aruz| &&
      |M/P+3uf3XL7P5QcODg4ODg4ODg4ODg4ODg4ODg4ODg4ODg4ODg4ODg4ODg4ODYcmtwSRSA| &&
      |MtQAfQCuSAFDDiliYYKbcEvmsyBZgHnAacDEwHuoBh4CDwFvA88DLwOnDILZsTrDBMA1YD| &&
      |7wWWAjOACdLshWuVBQ4DO4CngG8BTwN73RI6FJu7s4EvA29KM43EuHJ67WbgDuBUt1kdPD| &&
      |QD7weelCYaSXgNAt8HLpWgOjQwMsB18pWitNSwrlzEa56VcDW05mr0qHA1cAuwOEDLjAD9| &&
      |EryngFek1Tql6fx81uOAOcBrwDa3dxsPC4CHIjTVHuCzwJmKCqcCy4AvKRIM+rsB4B693q| &&
      |HB8CfA/gif6UvAsT5/Ow94MMQs5oCdMrOOK2wgnAz8JEJbbRftEBRFfhDYF/L3WeC7wOxG| &&
      |DbMbMQpcgZGfYfd/ANgd8Lsc0C2TF+a/niH/LeUEq/4xATgLS9GEYRJwTMDvUvKfWiPeYy| &&
      |qwXJ/pBKvOMQNYEuPepypqnOzzu5nAJVjqJwwtwEKgvRHNQqOhS5RAFDLAB7C0zYNyxlPy| &&
      |mX6XeERoCniHNN/bTrDqG226UjGEYhbw19JOL0qQlmB5xI6Yn9cZQ7M5waoDtMS4b49F3w| &&
      |FslSOflrO+Rc77fJnE5gghbWpEl6MRBatP14iPQHjk5s+BR4EfYwz6XmCoQJNNEWWxGrgA| &&
      |eLe0oB96dTnUORZjtVTF5GZOQvRJYFHMSK5VzvmnsAoHPz7rUeAEt+yNERV+m6PJ0QPAn5| &&
      |fgOxViinyxwxydlP5KQGTp6IY6w17gBT304p//FOhJ8J4HgLUYE19sBp+V6XWCVecYxNI5| &&
      |3UU/zxb4UUmQ9fHXNkuwhhptkRvReR+Rc/4j4FqOZM8zIU54FDJF/98PfAfY2Ij+RiMKFj| &&
      |JZ3wDOAU5SpNeFJZZ/ndJzeyOiHjxfKidz+5CLCBsPGeCv5Ft5UWG2zMuLNLcBv4erf29Y| &&
      |zAa+QJ7XKvfKYcWBt9KAbLvDkZgPfFGaK1emUL2FcVoz3bI6IEH4S2AT0Q0TfgI1hJGuHy| &&
      |EZD+ZQ5z7Xe4B7pXmyEQKWw9I/m4B/w/oSXduX4JzLI5FWdLgUOA9YiTVPFEfPAxgX9jiw| &&
      |Dqt82O+WzyEOJmD1WLt9tNVW4HKsUsIhYIc6+KO/gIooRg5L4wy5ZXKC5dyICqISzHsL+V| &&
      |RJC/lBGv0YKz3sHsOYKY02rX0rVnDo5UP7tP4jtSZYaazOe5Ec4RMxMrJTN9cD/Ar4Pzm+| &&
      |G8jzSA7lYSLwTuBdWIvbHAUkbVr3PVgV7HpRJL8ivI2tagRrCvCbWLPnWVj9U1qmwzMfhU| &&
      |2du8jPmPoBbsZUUrRKmK4GLpRwtRW4O4VTCHPYALnXgEeA+7H5FFW5sdPA+cBdWEdKXKLR| &&
      |y9HtBu7G+vCqpS39Ygl+8XfeAqyqorWfgxUabpD2ycVYe+81fcAzwA0E91GOGzLANfqCQy| &&
      |RPjQxJe11DdTR5VrtgpbBS67sUpSZd9xzWOPL3VFEZdQvwYWzETy6GZhqK2FE54JcSrrQT| &&
      |rFAsAh7QmkZNHRyKYUV6sbzprGrwsVYDH8PGAgWF4F4ubYNubj42GqgjYBeeitWf7wN+iJ| &&
      |tQ7IdZ2MSc94W4DjltgufltM+SHzY94Fm1YcWPO4DPYTX844L5RM+Y6gH+FWuXateXny3B| &&
      |2Rqyg4bkVJ7gNJavlfg4eQI3aEbXg1hqapLWvQu4CqvPD9NcW/S6cUGTdswBwkf53BugWj| &&
      |uxoWYDIX9/CPizcfS3qlWwTsMaP8LcifUSKr8g67exYsSw5/aQIvrEkVxSzJNUTwp5TS82| &&
      |8LXb53f7RS+8HWLq2oErFTo7GCaIzjktxPUYViC1PsA8rhW9EKY0VgG/kVRGyhGsZUSPnx| &&
      |4IECoPuzi6ZarY31oov8ClnwzTgTWEj1DKYoPj+gN+f0BrH4Yp4sPaKylYTXrYUUVtGflI| &&
      |QcI3Q3Y/6gbPwRjlRkdakeCCiA3dDBwfsmbHED1xJ6UAa14lBWu6+JMoInMiNu7neJ/fTZ| &&
      |WqnUr0UI25VF8N+XhEqs3YtJupMV53NpZO8/vd+RLQKMGapbWvGN3gSXxUhr8Jq8p8G7gT| &&
      |yw1m5TNdKycyTk3TsfrM7go/yFSIUI1HdUMaO5YlznNbqMixDStK7NUaXg7cRLwZYS3YfK| &&
      |8mSiwWSCpYmRJMUwb4fZmzjaIR5krjTYz5gCZTmVryJpnmE2VuLsB/7kIX8Fv6d5Nok0MV| &&
      |0GItxJ8D0QRcBJwiHuuALI03WjxVoiBXRLBadaUiTMVB4A1pqq36f6/vbou4rXdqsZoi3m| &&
      |ssH1qrBGkNRvgu0e5uD3CSJwMfIj/x72dYZ/Va3Vt2DDVWlIb3OMAdwKtYRmSXfva6NsJJ| &&
      |ut9jYzzHdBLtnFSwerVD/cyhR4o+hbWYr8MOPuopWPBmPbSZii4vk90/JuAmBhmbzHta/t| &&
      |8HZCIW63vFGQE5QddUmZ33YnMavgU8jI2WHIvNEKY5POH5DvCYKIU95LlC73t3SZOt0X3P| &&
      |CxDYEdFCWSqEOdqdwz7E3OvAzXpNHEn3jgm5CXgJf9JvF3AbVobTNopC9R4JQT/l9RQW3v| &&
      |9B4GtYI8ZoOu0nA38ksxaU57tH/FPcWvwWaeiv4U9U98kXrhg6gP/Cf8bUHxM9pjrIHN2I| &&
      |P5Of08PfiHUuvw9LCyWNar3DmV4kus0r6Ulg6zACuTXhd0zJv7kA+DQ2yOQQ/umzHEZEL0| &&
      |j4WYtkyovfcytWxlRR3OQjBNuAc8t4z2VYRWPUQ+vGTn34GMZAl+LYT5NG3TIGAlU8dO0l| &&
      |rPJjcolCPxebzHyvLECURh0E/qkMrm+Sovbi7/9dRYUVoxvAThTdypHsu9cVXI7/kIuhvm| &&
      |eKXF0l5/QJrBLyOfkEuRCy9Sbgo1qwOKZ6pOjfVAQVUWhqT8EGj7Rh5/IMhGinNq3lxdJS| &&
      |p+n7pmN+x54yfDq/59YvLbav0oL1stTvSRw9Y2oCyUYBZUo0Fe0Kn0+XA75W3+mJAo1UiC| &&
      |sUzR1bwnfqUXT1doF5WhBTS6awzMONio4f9vn9sdLUl0qgjtfmSZUoGNkyzG5bUVQ+gtXE| &&
      |fS+p416OYPUD35STeo5+NpV8HVUSweokzyp71Q2ZiJA4pUU5DktYXySh/1/tuJekxVZKU8| &&
      |0qQXs+J8f2e4quPMG6XE7tmTEjyHkSrs3y6ybK3K3Esg9Ldd9NMdatsPJjooSwRZtmfkKt| &&
      |1aTAyMM+meFXGCe0yofoJl8VOlzm5fkS24Hb5bg+hdUelVJH36eFuRv4Q22CgRKiux8pYm| &&
      |oOuO+LsQqCuH5avwKPK7ES4Gflo5Zy/vRhBTBflX/5sPyrkVFYe+8+BoB/xz8NV1FMAv6C| &&
      |8scAFV57gb+Vj9Eqc/sh4Oty7qPKm4ud0H2El+8WP8BtWLqpKcLJ/ojeO1eCYGwvEIa4h5| &&
      |kflHm/RcHRJGm2s6WZh0dp3bPA/2CnllUF2iUI20aBB9opzmpaAON9EVY2+5we1GhHdjkJ| &&
      |8LQY9z2zwA8Z7e8wJNP5n6JGjvcxuyn5ZT8oQVjDNOr9Etyq6vKeguUEnybfglTKQvZjVZ| &&
      |E3EF1KM1Hcy0ex8ts3C9j50aAJbo3pCLcC/1CCiY2jnfYoCLlNUW9XhB+XVgT5+QKXpNT7| &&
      |fRO4g+hynHFDi9Tz7WKIDxYQkDmO7GfzFvKQHNrP6m9LCSjSoh0uFY+zXp85XIaQZYG/i8| &&
      |leN0kABsoUqH5FjV/X5jypxAgZ0SfXK52znSP7C4vXflg+6FvSUu+PqaFLCtnHyqk/VdHi| &&
      |UvE50woIvH6F75vEUHulsv1lfuZJ2uUXS6V3UnoSNQd8GfgE0YcJdGJD165JEGF75uclmb| &&
      |LHtTH2l8FHefTFmQo8Tte6d4pS6NdG7lbwsE6M/u7RFoCxVntpPfAZ2lHt+sxe3cwO7Zzc| &&
      |KH+md7LpdViCu7XEB/5LLC+3NmLtLlSkN7fEtczKGb9fArWF0R+J1CL3pFPmdKLW+qDWvq| &&
      |LJ5XrDzRx9vk3cXN+dESH3fPzzpXGug8CfUsdz9uv5AIFMAemYZLdfpR39VZnpoQKTe6b4| &&
      |u8tJlghvUXYi6wSr9tAkU5DU3HcBf6CA4kmRrV7X0GqsjCXp8JJ0wXcbcYJVW2ghfulzkB| &&
      |M/gtWVXYEd3+sVyrXLBKY4cjxTKb5th7TfgBOs2sKwfKVSI7Ve8TrrsVkTm7BCQ+9MnA4F| &&
      |InPFpZ2B1YZ1JHDg63bQXL0LVtxSEk+gfoYx6U9iFQ0HCzRXscZJY5mA+aI2LtO/cQ4y96| &&
      |om3HDcGkQKy2H2Es08P4/VTZ2Y0HR65TG3yNGPihR7sSoQhxrFteRnQwSx7N/HhmdkRuHz| &&
      |Mhg5+wThzP8OxnGai0P5OEO+UtAD7sZmdo52NPphwkcMrceyEXWLeh+08QbGogc5ydsJn7| &&
      |qS1Ld7Bau1Cvr9JqyKwwlWjWK/zFJQzm8wQeQYB0MEk58HsSLCvU6wahee2dlE9RCRGyTs| &&
      |I06wahsbse7kvpCILj3KVxCj3odVMrxe74veCIeNH8YOFl+BVSMUbqapWL5v+yhushzGbb| &&
      |X7/HwdVpjY5+Kq+kAK+B1FgcUcVv8YXMUVtDmsVv/aBrESDaGxPOyS4zyzyBXIVOjzd2JT| &&
      |d9x5QXXkRy7DWqVGoza9nGaFu0k4Ic+h+nA2Vgc+PI5CVcj03y0fzKGGsRzrkwvqKRyroS| &&
      |BhPY992Nk3J7vHU5vmbzk2LSXI/A0AL2As+RCjNx9rAGPzN4S852GsMvXX3KOqLaE6V0IV| &&
      |1MQ5JJ/rPKzh804sMZxUwLwG027seJeVWDL6cYKbWfuBrzjhqh2swGqqws7oeYgjh2BMwk| &&
      |57uBMjVAdj+GSF/XkvYg2jl5JvcUthJcyPhbzXADbeqO4c+no7MPs8rK7qAvwbTgexMpnb| &&
      |sH66kaK16MB6E5dh1QcLsSk2mYL3GyTf7LlB17NY2qjX5z1XAX8jIfPjsHqwbp/P6T0cqt| &&
      |CneiTEpxqUpnp3jPdrkpDNlKC9S0K7EivFmY/1SsYZhOudPPtoiObqkbY80T3K6sJZohSy| &&
      |Iebvv4vMX6UFfxWWJ8yGmMW7HM81tg+hVRpjApYdCDPZy7Ak83CIpnoAazcfb7fjPAnXcI| &&
      |RDvyBifbwDHLxOn6pLE1VLSmcy1ulyqnbsTDnUAwrNt4oa2CzfxmtCOBs7ZPuSgMUdFI/1| &&
      |aayufTwxgjVp3Kr/vtDnO2ewuVxgU2xe039PwNrQFsr3my3BapIZ3SXa5GWsuPFQo2unTq| &&
      |y64AvYvKvdcoCz2tVZXT0Srh9i58MswgaOfDvCp3pA/lG1aeSV2MC0ILPYizH0i6SRb8fm| &&
      |SLxVtD7eGg1o7dZjQ0ouofoOtaqYWViGHWzdTWmDy3olhL+IMH/3VYH5C7v/FYpQw3yuZ7| &&
      |DEdSn8WlZa6w7s6JaG8qGuwmqTyiEkcyEP5JvY8SXVjuUSruEE9xm1PoPSipeRfBRAzSAD| &&
      |fJDo3juPfCx1Yb1JzmfUyHqkYghXmMBFDZjLyjxeTfxjUGpSU11OeA4tJ1P3Isae/zimqf| &&
      |RydNVs/sKEy0tBxRnAOyR/8zHxdq8QfnJFDhvfuYb6I8RB9v6RCOE4AHxGwjFV0eGVErKw| &&
      |HT2MtcefXsPrsxxrVQsTqgGstPkyLCMwDSN8/xn/M4gK1/Zh6rBcpxX4FOHt7oexOaSTfH| &&
      |b0Odjg27Bd+RrGcNfirvSmA26OEI7HsdlcxZiOjbcMm5y8D5sNX1da60xFcmH+wMvipfww| &&
      |UVzUQITW+rwojFpDF1ZGMxix8W4O4R4v5eiafj+TWJE6sEoxtquxnFvYbnkLmxPvB+9Iuf| &&
      |6IezmP2sy3naI1CnOwB7BZpUGNsN2EH6iU0ucsr0SUWAnB6pQpizrAchLBBx81y5+IimxO| &&
      |kJ9VS00irdhk6ZkRr2v2cRMK0RFjjdslWBPqQbBOwBjkqM86URrH73UzMbY6qqOmAxum31| &&
      |pDgtUuzi3q3iYqspsVQOOskK8V9byXEP+gqqoVrBRWXnJcDKexC5vScgV2NrQ3AHYudlrF| &&
      |+TG+b5MWra2GBGuyvnMqxr1dgp0pNIf8qWjTsDnz1xPvIMxpjPJhAUHqdawFqyOmTU9heb| &&
      |1/xPJiG/W3S2VK22N+5hQt8J4aEayJxM/rvQPLlS7FiM9BLHG/Qlo9FWONJ1ciwKmEYMU9| &&
      |FHMQy9If1iKlC6K9N/Szzhhaq5naYpkzMTVsTs75Lmn0k7HkfAbrsgY7laKlGtZnrAUrp5| &&
      |sP6v710jDrsQrLJxX57Cc/XqhFvsMSrFhuFUb0tYQI6GANCVYf4WUuWfFbP9EabcCmFHon| &&
      |enhjxxdgDP4arKDRb2K0t9699cBhnYsN3fDjnTZi9eCLY0Qqnlk9HyspORDAi92nHV0rmI| &&
      |GVTAeRmvdg5OnkGJq/VQL2Ca1tzofL2kIwX1hTmI81LhTf5E45okm05mzgPzg6hziIFcjV| &&
      |kvPegRG7xeszRPSxK2Hm9QaOHlfpnRxb81EhWML0pxxN7G3FjuRNcuzHDqxWqVilH5RZra| &&
      |Wh/P3aeMXmsEem780E7zmAsew7fczqCzKlNS9Yg1jt1QGfBU3qC3mt6iNFP3tVC1dLE12y| &&
      |WNHiVp/7OVzmuhfP4dorX23MfdBKMdTPSJNcUCDMKTngzQm/d5PPQj6iCLLWsBmryVpAni| &&
      |gd0X0mJXtbi3yyrIRqLRUYU1kpwdoE3IsV4E0rcFqvw2q1S9WcKUU+LQUP4WmsxasWI54e| &&
      |BR1rMC4vJT/xaiyBn6QiYRpGTBe6D99gDA69rIbo5y7yxWzeNL2+hFfh1Lwt2LS8Wh4k14| &&
      |QdvrmdI8uLB8q4vBq2HqzTum4bLJYQ3lia5NqFtYC118H6dGHt/3sZvRFLg9hZ03U9k8ub| &&
      |ZfCItFW5I4N2A58kOvlaS5gjymRfmcLllXnfx/h1gI+L5voixrAnWbwsVhj4ccJLSWoVk3| &&
      |Vvr5J8EuEu4F+AeTQYpgM3SnvtJbrjxGuY2CShXEltlceUigxwkfzSN4hulctpw+3BGjOu| &&
      |pwJVDGGmaTzRjDHLq7BarMUYq+7NI0jLR+jBhu6vw051+Dl1fmRIwfPpwlIw5yuqnke+Yi| &&
      |QtgRvEEtG/wEjntRixOtyoglX4PSZJi83RTvOqSXfq2qErS2OiFatemK316dTP9srX3Em+| &&
      |BX8EBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBweHiuL/AUoCeZ| &&
      |bSvjrSAAAAAElFTkSuQmCC|.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = lv_imagebase64
      IMPORTING
        output = ev_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_filename = |image_{ iv_otype }_{ iv_objid }.png|.
      ev_mimetype = |image/png|.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_IMAGE_M.
    DATA(lv_imagebase64) =
      |iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAAAA8AXHiAAAABmJLR0QA/wD/AP+gvaeTAAA| &&
      |ACXBIWXMAAA7DAAAOwwHHb6hkAAAAB3RJTUUH5QIMCwsUTzAuHgAABxZJREFUeNrt3WnIXN| &&
      |Udx/HvPFuM2UxMXKJV1LpSldoN627dFcUNcQFb3wgKLfSNb/q+7yrSalqsooJboVYNoSqCe| &&
      |3AtuKCtGvWBxDxma2ISnyzPzPji/IdEiZl779z7ODfP9wODIXHm3HvP755z7rkz54IkSZIk| &&
      |SZIkSZIkSZIkSZIkSZIkSZIk9ZvGJJQxHzgSOAKYCwx62CddE1gPLAM+BNbUOVgN4KfAr4H| &&
      |TgAOA6ZMUZn1TG9gCjAEvAfcDbwCtOu7MicCTsUOteLV9fW+vTh1sAZYAJ1VZ+VV1SzOAW4| &&
      |Frgb2ilbKl+v6HPQ1gCPgBsA1YGv8t3UBFO3EgcFaESv1nWtTPwVUVMFDRmTE/wmUr1b+t1| &&
      |/5RT406BWtWDNTVv0aAvesUrMmaxlAf19OAx1YGSwZLBksyWDJYMliSwZLBksGSDJYMlgyW| &&
      |ZLBksGSwJIMlgyWDJRksGSxNJUM13/4t8WrGSTJC+pFsWb/wbgPjwFbSz9MHSD/2LHMNila| &&
      |UsS3+PBj7sJfBmlxt4CvgHeB14D1gQ4TqMOBnwM+B/Xpokdvxmf8BXgP+G5U/HTgG+AXwE2| &&
      |B2DwFrAatJi3O8AXwS4ZoJ/Cj24UTScgUNg1V9qD4F/hGvZcDG+HviLF8AnAvcCJxaIFwt4| &&
      |F3gIeAJYDmwead/nwEcBFxOWpvi+IJlLAUeAJ4GVkXL2zErTpKrgWtIS0BN+WHLAHBenI1l| &&
      |r5ayDLgZ2LfLNkwDTgEWRyuQtYwm8BZwVVTu7swErohWcyJHGduBp4AzMnR3c4GbgP9R/mo| &&
      |9a4AL6hTYqoK1Bvh9hCbPdryVI7ijwG+A4YxlDAPXR+CzVvw7wMU5KnQE+F3sf22CVZe0to| &&
      |BXgMdjIJ31PS8C/4wxWbvL/9+MbmlxtCpZbAf+Ha+JjBcC/wKeI/uiZ9tim16hRgul1SFYn| &&
      |Qp5NlqUvFeNzwCfZShjbQQr7zKK66JrW5UhvMujjK9yljEa+z+eoQyDlcNYdCHNAu/9LK64| &&
      |up3tK+IKs4j34/3dWtDR6Dbzasb+r7TFKtf/Y8xWxFbg8y6hbMfV5doetm9dl9akFcEYL1j| &&
      |G6ijHYJXcHfYyvmhl7ELaPXx+ljKak7APBiuH2cC8gu8dIk2W7m42vkGa/JzTw/bN6XI8B0| &&
      |ir6I0ULGNelGOwSnQgcGzB7V0IHE732zwLgaMLbt9R8f5ux/pQ0sKyRerp2DgOBqskDdJk5| &&
      |NmkWfW8rdWZpFnsbmXsB/wqyspj73jfQrrfejmEtKjscM4yFsT+z6Imt3fq0mI1IiDnk+8G| &&
      |849Js+hZ7ukNAxdFxQ/kOH5nAJdk6OIapNtBV5DuZ+apo/Ni/6f0EpxVzbw3gTeBK6OV2J1| &&
      |B4DjgPnZMjma93fJMhKvbDP800hM3lpDvttE48CBwAt3v1U4HLgNejf33lk4FweqE623gt9| &&
      |G9jeyi7LnAhaQZ7m0FypggzdhfF93j0HdcDFwHPE+++4Q7l7E4Wrp5u2iFR2I8dgvpllSzg| &&
      |mNZabCGatgaHg/8IQ7KC6SHDo3HvhwAnEy6AX1Ywf0bBH4ZFXsp6VbKaIS0U+GnRDkLKfbd| &&
      |r8E4+Y4mfcthacxxTZBuTP8QOD26zAXU8JsNjYoq/5xo7udXvP2bIlTN2JdpMY4ZLunzm6S| &&
      |vzHSeBzQQFT+D8r5MuD3K+PYX/WZWPKZaC9wQXX9rqrdY3zazwFVc3pZldsXzR8PAPnviQF| &&
      |syWDJYMliSwZLBksGSDJYMlgyWZLBksGSwJIMlgyWDJRksGSwZLMlgyWDJYEn9Fqw2O34jp| &&
      |/7Vovu6qX0XrHXUaPW5KWoTsL6qBqCqrnAV8IGtVt9qR/2M1W2MNUZ6csRyarS84RQK1Qrg| &&
      |UbovyFvYYIU7sCJarIPYsQ5Be6edy/tUio5GyQd5589v76GvzhqpW4GPgHuAR8i/LHhmVS/| &&
      |kNYP0PJvTSQ83ml+wlWyT1k84huxPpsg6Fvw4Ljb25EXNWqRFQD4gLdH0Mt98PlDtgtUxQl| &&
      |qOp5elDk8C/kixNTx3FarlwF9J61RNsGdrx2B9Ndmf7DFlnEp68lcZ3cJK4Dbyr2mqjOq0j| &&
      |FGjpDN3BXAncBfpoQGq0VVhv3YHXwCLgHsNlS1WWaFaCfw5grXBqrfFKiNUayJQ9xgqg1Xm| &&
      |lMLf4rXaKrcrLCNUa6OVutNQGayyQrUeuDuu/sasaoNVVkt1L3BHXAnKYPUcqi+j+7vLUBm| &&
      |ssqwH/g78ifTVHXlV2LNNpKdhLDJUyuM0vvte4QbS5OehHiZbrDJbqodioD5qlaqMFmsj8B| &&
      |fSo3nl4L0UG4GHgdsjcFLPLdbmGKQf5WFRWcHaDNwPHMkUf0ayXWE5GqQHUz5Gmqf6yOrrX| &&
      |4M12tb9oxtcBLxn1aksc4CD7f4kSZIkSZIkSZIkSZIkSZIkSZIkSZIkScV8DYAIGmKsbMgR| &&
      |AAAAAElFTkSuQmCC|.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = lv_imagebase64
      IMPORTING
        output = ev_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_filename = |image_{ iv_otype }_{ iv_objid }.png|.
      ev_mimetype = |image/png|.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_IMAGE_O.
    DATA(lv_imagebase64) =
      |iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAAAA8AXHiAAAABmJLR0QA/wD/AP+gvaeTAA| &&
      |AACXBIWXMAAA7DAAAOwwHHb6hkAAAAB3RJTUUH5QIPEAoIQOygbwAACPFJREFUeNrtnfuP| &&
      |XGUZxz87O7PddntFDbhUC0hV0EqLSluMPxStGhWLAv4kkRAjajRe0hgh/gEkxgs06i/gBc| &&
      |UgxF8kAcQmipFLrajQYsAWSrFiI12WbffC7syc8YfnOc7xdGaZ6zlnzn4/yZtuz5w587zv| &&
      |+Z7nfc97eV4QQgghhBBCCCGEEEIIIYQQQgghhBBCCCGEEEIIIYQQQgghxFJhKAd5KADnAR| &&
      |tykh+AGnAUeBYIBjEDxRzchFHgKuAzQCknwioDtwK3ALMSVnpedy2wHhjJibAWPE8D64GL| &&
      |ObkRtRxV7bmgoCIQEpaQsISEJYSEJSQsIWEJIWEJCUtIWEJIWELCEhKWEBKWkLCEhCWEhC| &&
      |WyS15mkAbYdN5aTvJTBqoSVvo34SFg2FMeqHqeyoOagbzMES95yovHGnJRlRFCCCGE2lj5| &&
      |Lqd4WdVy1KaTsFLgDODt2FL+KLPAE8BJFZHohMuAfcCxWHoQ2KriaUxRRfCqlIAzgbNjx0| &&
      |+q/JqjIZ3WqLV4TEhYQsISEpYQEpaQsISEJYSEJSQsIWEJCUsICSsVFpseo2EdCasjSsAb| &&
      |gRUNPlsFvIn87IYhEhTVx7EpMxX3ToGnmh/bB3xC4hKtMgzsAh7FlmKFovqPp1BcVT9nF/| &&
      |lZeib6xAhwBfBIxFNVgcPAVz0digiu4uK6gvzs5SN6TNGrtkdj1d9h4HPAmKfr/VgQE5eq| &&
      |RdFQVLu83RSt/p4GvhBrwK8APu+fRavFfe65lvzM0iTbBSXqy+CzlkaBjwDfAN7lxwLgCP| &&
      |Bd4BfAdCQvZa8OZ4ALsC3gCsBZ/qZ4AtvIkgznua8bbCa1Sucs4ErgNRl9wFYD7wc2Rbpg| &&
      |DgPfAn4KzDf53jLg08BuYKMfC4ADwF6yu4JnAvgVcLyf7j8pYX0WeAvZXXJWctvCNlXoqe| &&
      |YX+c68n1PwRv35/vc73JNlkZpX4Q/lQVihdxwh22sZA+AZ4NvAnbHqrxnTLq6qe65QXCMZ| &&
      |Flbf74F63v+fOeAe4PY2q7GTwM/8u3MqRgkrzii2CPWyNrsNSsAO/+6oilHCavSWvA24Ad| &&
      |jZYvkU/NwbgO2oBz7xNlazxu9+bMl6mowBFwPjLoxLXSgB8MAir+YF4AN+7qX+/xrwAvAX| &&
      |745Ik/XAu/3tdUkJ6xRwG/Br0o25uRK4GvgysMEFsg34pgvtAU6PrleKiGprRFRHgZuBu1| &&
      |ts/PfT+34MeGsawkqKzVhklrCXOkwvAp/MSPUxBnwJ6xSN9qb/Ees8jfNh/yzaS3/ErzGW| &&
      |kWr9amzQPFrmgd+LzXn2WFlixt/sii6Oc9wLbXWvVAN+6+fuBG6MearngD3AzzNQBS75Nl| &&
      |bWeBn4kVd7X3NxFV1AN1Lvm9rtx4oRUX3HRfWyilHCasQU8GP3RF+JiGs7cJOfszHmqW72| &&
      |78yo+CSsVqrFQqxBvzHyNhgAz3v1d7tEdfrrsmjMJHAr8MNI9VaIlNkEcIufM6nikrDaYR| &&
      |r4m1ePcU4Af/IuEyFhtU2zAVst/ZKwhIQlJCwhJCwhYQkJSwgJS0hYQsISQsISORRW3vZG| &&
      |HJLtzUly2swr2OaRUebI/phbzW2faZCfQbY9F8L6F/B9To/dMAc8Tp8DVHTJ09js0NWx4y| &&
      |9hc9yzSji3/SZgeeyzCb8nuXDnzZbXL2T8yR+iHtch7g3KA2D7SBNPtqCWoBBCJF0VlrCF| &&
      |oWG1UvHG/CAE0ViGrRUM4zmUvUE8PwC2L8ciEBYj1fc0py/AHThhrQMuAi4BtgCv826OKe| &&
      |DvwJ89HctYe2XIbX0nFuXvIuCMSMP9Cbf7MeqLQrNk+9lY2IBLgLcBa7xB/yI23Xq///vS| &&
      |oHnDcD3eHn+zmqYeMDZcZTyHxTm4C/go2VhBHL5o7MDWGD7n3ilqe+htjwI/8XOzEg9rDL| &&
      |gci+91zMu4GrN9Ggt1uceFNzCrtQqeub2Rt77FUhU4CHwR2/Eh7WrvU1gU5KAF2wMsqO01| &&
      |pB8jYaWX4cGYmJqlBb9HlzMgozA7gIdbzFw0PQtcl+LTX8R2ozjQpu1V4EngqhSf/hJwrZ| &&
      |dhrU3bHwHeR8ZHE8aBO1r0VI3SfuA9KWVyE3B/i56qkefa622xNNpU29zLdlLmC8AvgTdk| &&
      |VVTDWBDbyQ5vTpjJRj3dSXir3dji1FqHaQr4egpea42XWacPc+D5vp4eRf7pdb26zl3q6i| &&
      |48TtGr0nNS8LQ7u2zjrcTCeo8nbPsGL7NiFx5vFRYic10WhTWOBfrq5rpDWCS6NyfYoBxy| &&
      |IZ/X5W8WgHP9WklV5QUvq/Vd/mbB7914FoV1prvlbhn1ghpO8Oa8vkdvpKv8Wkk9FMNeVr| &&
      |0IqrsGi8mfOWEt61H7YhjrMU7SY63oke1Fv1aSHmt5jx7CYq+6THp942bozah5BQu2kdR0| &&
      |mpr/Xi9sX/BrJdUTH/jvVXpk+0wWhXUcm+vTbaGGvdqVBG/OMRpHlWmXKb9WUg9Fxctqtg| &&
      |cP1wTw76wK62CXhVrDth05lOBTH0Y7foruojdXseGrownbfoh6x2g3D9eBrAprCvgNNtDZ| &&
      |aSbnsSCy/0z4lf04cC/dDcpOAvfRx82PmvC8l/srXYhzwss9qzuWsRaLdDdD+x11FSym+q| &&
      |aUbD8X23Cp3KHtd2H7FabBhf5gVDqwfRb4Adnd9u9/bME2BVig9R74KjYFZRfpDYgOAe8F| &&
      |fkd7vdgLwINYJ2Va423hwP9jtD7OGbjt92DTgwaC7dj406kWxDXvN+ZK0t9TeRj4oD/9s6| &&
      |9ie+Dn3At8iPSnn5SwTQP+QH0V0WLpFDa9Zls/CrFfvIBNJDuBdd4ti4gmcJd9Ett08m5s| &&
      |48nf0+eZjS22N45gE/lOYfObRqjPwAxtn/SG+h3A97AZAtWUbQ+8If+kC36F2z4csb1Mfa| &&
      |LibdjqqYO9ftlIwm2PYEMlm7GZjK+lPoP0MPBXv0FZbDSOYsMlW7DhjugM0qf8wfkH2Zxe| &&
      |vcptvxjbnHOtC+uEC+9xf5MchOnVLVEku7uPtlLVlAbU9hEU118IIYQQQgghhBBCCCGEEE| &&
      |IIIYQQQgghhBBCCCGEEEIIIZY6/wVGbuosugb8agAAAABJRU5ErkJggg==|.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = lv_imagebase64
      IMPORTING
        output = ev_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_filename = |image_{ iv_otype }_{ iv_objid }.png|.
      ev_mimetype = |image/png|.
    ENDIF.
  ENDMETHOD.


  METHOD create_image_p.
    DATA(lv_imagebase64) =
      |iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAAAA8AXHiAAAABmJLR0QA/wD/AP+gvaeTAA| &&
      |AACXBIWXMAAA7DAAAOwwHHb6hkAAAAB3RJTUUH5QIPCiMW7xQkQQAADJNJREFUeNrtnVmQ| &&
      |HWUVx3/33tmSyQyZQBKyMSRAHCRASEJUwCJSCqiUgiCKYpWWolb5oLzgg+WLlg+WiuJSqK| &&
      |VoypKlDLgWCCWLGMIiiIhAglkIMRsTsk1mJpnJzPXh/Lvmmrp3pudOd093z/lVdZGQTLr7| &&
      |9L+/73znnO80OI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI7jOI| &&
      |7jOI7jOI7jOPmnlMJragBagDIw7I8om7YqpOAa2oEu4FxgETATaAX6gAPADuBFYBNwaIoL| &&
      |KbDVecBCoAOYDvQCBytstRE4PFWFdQqwBrhMoloAtAGNehOPA4NAD7AT+DfwqI7uKSao8d| &&
      |rqRdnpsalmq4uB78gAPRrKxzp6JK5b9fNThYsmaKtLpoKRmoH3AX/W0F2u4zion3+//Iu8| &&
      |2+qBCdrqwbzbqgG4BvibHM3yBI5hYANwHdCUY1s9EYGtyvp3rtXUmTsukajKER4b5HsU3F| &&
      |ahxHVp3kTVCdwVsaGC4zfAErdVaFstztOwfjNwJCZj9QK35GRKbJStemKyVV+ObMXSGIb1| &&
      |E48ngXNyYKuuBGy1ATg77hspJhAnezewKubzXABckcD9xP0sLk/AViuA98Ttl8b9INoV1G| &&
      |uO+TxNcnhPyrCw2nQPLQnY6jIsw5FZYS0CliWwaitgEeksO6anY6kaErDVMuC0LAtrCTAv| &&
      |oQczV+fLYuihoJfi1ITOdypwRlaFVQBmJ7gCaZbBshrTOjUBl6FypT4nTlsVYxbtbJIrzS| &&
      |nJb8jqiNVBcpHxIpbYLmZRWCT4BgZkNWVRINnYUkHPJpMjVhmrCSonZKwy0J/g+aK+9l6S| &&
      |K9YbZiQImzlhDQP7EjTWcWB/hoX1pu4hyfMNZ1FYALv0JiZBL/DfDAtrZ4K26ovbVnEL6z| &&
      |UdSbAD2Ex22QJsT+hc24GtWQ03oLfw7wmMImXgOZ0vq+wCns2LreIW1lHgYflacfKmztOb| &&
      |YWEdBR4h/hr1wFZ9ZJx5wDqiqYSsVU36O2zXSpzL8wYdhRzYakHcDz2J4GVQh/UO4kkS7w| &&
      |K+i5WDRGmXdmA+VsrSheXXlkrAc7CYWVkruXKEtkK2ao/JVrdiZUax0pDQqPUgVq7xBSyL| &&
      |H+VK8C5sw0FUrsFM4K3AamClRDWr4rp7NJ1sAv4BPA28jG1cGI7YVjMittWd2EaUXHEG8G| &&
      |tGgpgTPfqBezSKREEzsBz4GvACMBDiGgaAfwFf189GlWk4UyLoi9BWdwNnkVNWaoTpnYAf| &&
      |MSyD36NRJSpRXSfnebCOaxrUz14XobhWVYhrIrbqlahWk3OWAt8H9gBD4zTUELAX+AHwlg| &&
      |hF9RlNZxMdGV4BPkt0xXpLda9psVWqKchnuRG4H+vHMBTizTss/+AT+vlCRNdyIxZYHYpA| &&
      |WENYoPOTEV7fibYaDmmrByK21bgvfLJoxCpM346V5K7EapKCngQDGsb3KKD3hFYzr2vqiY| &&
      |I1wDc17UQV0xsGnge+rHhRXLaahzVPaWKkb8NeLMj6BPBUxLbKjLCC8zdp9TMXq0k6SVPJ| &&
      |0YoV2F4txQciXNrPxnoifJToy20GFY+6Wdceh61O1ksY2OoQ1p0nDltlfoosyXjN+m8pRv| &&
      |HfoIcQ1zarbk1hebCVM47R6u6I/KrR/K11Gl2mHA0pu5ag31Nlz6cBoq8bOlv+Styl2W/D| &&
      |NtLuTchWgxL0lBRWk3ypuVjKJPCtZgHT9OdNwDEdvfKz3pAjv0e/7qlTcCXgQpLZETNH5/| &&
      |prnQ+80lYL9O8FtmqpmAYH5Gf1aQrulph3y1ZJVvImKqw2LMd2ht7gxYzk3Dq0upmm6ynp| &&
      |bR/SMSiDHZRzuhurvdqOpVW2y4gHQ15LC3A+ydSYN+pc0/UihGGGVoCBrU7X70+0VUn2Ku| &&
      |oFO66jt4qtXgNela12k0AbyTiF1aK37Cwt58/Vr0+TgcYzDc3i/6sXjmFlyDsUN9qK9d3c| &&
      |qN8fGOPBLUzw5V2oc/aMIcCFWEB0FbZxNbDVTMZXLNAxhq1exdJQW/T/9mdFWB3YxtFV8j| &&
      |HOk8GiTD43K44zD0tVHNOb+DKWFH5GxttdZQpo1TUmRTDKVKMVa1u0SvGp5ViUvD1GW/Vr| &&
      |BNsE/FNxr5dkq2NpXAicLIf4Fizq+wbxdk0ZLeH6PPDFGtPd2TLqcALXMqyR4Zwavt4HsJ| &&
      |zn65Nkq2N6AX+i0EiXptlU0IZ1e/kK8Ljm+HIKjvuoXnayFGv8mpSwNuqBVRutfpUSWwWb| &&
      |Oe5RfG8REwwaN0xwiF2ItQ/6kIby5hSNoE01/Lgj43D0o+Ag1Uumi6SrO8584HqsyPB+xe| &&
      |CeqdfRr0dYBY1SazR8Xk462we11hB6L8n2Pu+uIawmrRbTxiLgc3Jr1mKlO93jDe2MN0BY| &&
      |UqjgS8C3gA+T3p5Uc+T3nUifpqckAonBVHikxkp3NunlfOCrcnG6iLGMvaDpbh3RVTbGeb| &&
      |yBda6rxjUKusZ9DQf08lVjTcWqNc3HILYB453ElIu8EHiIkc0DaT+OamStNiovIZp+82M5| &&
      |7k9SvRy4CHw+Iy9okPd8SBqIdCqcDnwc6xNeIhs0KW5TLYb0OvDHmOM2A8CfqL4TvEUPKS| &&
      |tfiyjq2d84SkyuLlYrYlvO2LGR2s1iu7AcXlznXk/tTs7nKDg5nDF7bpFTH8mIVVBEeCHZ| &&
      |o1N+VrX73Az8EmuOETW7tKLaVMOe78J24mStfmo+lkkpRiWszpTFqMLSgn3oqJqfc1xO6d| &&
      |oaq7Z66VXg8z6qtyVaoGtqzaA9m6SFQhTCKjFJBfkRcYFWgdUiyQeAnwG/0CpxouzXKPjT| &&
      |Uf69K8NOJyklVM63GPLvZPkTGa3AR7Dv/lXjNeA24MfyIeplC3A7tt2/Voug5VoEdWTYnq| &&
      |EWfA1MDZYBN2k1uG0UUWzV6LZaAdYw7MNSH/di2+N3juKffJopsHF0KgmrAask2Ax8j+q5| &&
      |wp1YC4AXsM+0rJaDPUfhlqaKMEIfFoDdjJWd/AWrEqgVvmjXUv0G0pnGcWHVyaCc6k495F| &&
      |pJ6GPYHsaXscrNLiyYejIjqatD8p+C4sLtjN1ragZWERrUpTe6sLJN5ce3n8LqtMI46f3Y| &&
      |dvlXKlZD0yr+bGCc13EA61mxCSt+PBcrvJvpwsoWh7H41HpsN3KwK7heBuoQ04lCfUzHIi| &&
      |zqfimWL+wk2x+XmhLC6pOAHsH6PKxn9Pr3MCugokItQbilMhdYzy6hHToeAi4G3ot9jWsx| &&
      |0fbDcmFF5EMFvTXv1sgwnqBn0AqyEQuqTle8pl0Pu7nCeT+q0esIFrc6ohHpKCM7i8JstT| &&
      |qiVeTjCoVcL5HNJQdfQM26sIIa92ewgrTfa7UWdjQqSUTBl8POxGril2C1Uh2MNCkpVYg4| &&
      |aMLRjaVv/iNHfqum4H0VQhtrVOvXC/Eslrv8GFai0kaGt8xnWVjD2ObVO7G0zEshRoqCBD| &&
      |VdU88KrBR3ucQUiChMEHCuhBhMiwMaNbcpZPEclmjephXpWCPZId3LU1gQ9VPyv4ourGRF| &&
      |tR74OZbvC/NdmEZsI8VFOlZi+/ZaK3ypeggadEzDEvXzJdZerUhfwOqyNmhUG6tl+Dbg21| &&
      |pB3iT/K8ufJK5Jsx5gWko8+rD83sqQ8aBmCekbmjL3K16VRMHicY1kB7GdQbcDHyRcVL8B| &&
      |y3OuJT3FlUPAD4moJq8ZuCMlN9aP5fU6Q9xcSUv6WzVqHKa+/qJRiqwXy03ei3XbOyXEPS| &&
      |zFEttpebF/FEZYYafCPvkAk+lMlrGKzNsY2eBZi2VYCuda+UEzUjCdBAuFTvlnqxTHug94| &&
      |lOrR+yFG0lALmPw8Y5mQX7QII5SiAnpLUnBTLzLShLYas4CrsJzcCmL+imgE9GgEW4flKb| &&
      |eMMgCslignmyA/Wp6osIIbK6VAWKNFv1dg5TFXa8WXpXzcHiygewe2yaPWiJeGxdYQyX1X| &&
      |cdJXtldhmyO6yV5dfnAcxqokriab1bq54wqsS/DxDIuqMk30NNayoOiPdvJYDPw2B4I68X| &&
      |hYC5DMUsz4tV+pAGLeuAjbcNHgwkqedqzSsz2HwmpRDO4kF1byzMY+/5bHJvllrOJ0lgsr| &&
      |edqwgGMem+YXNGq1urCSZzr5Lq0uZPn5FHNgfHIuLheW47iwHBeW48JyHBeW48JyXFiO48| &&
      |JyXFiOC8txYfn1+73FQZaTuMFWpP6cCiv4aoULK2H2AX8g241ix7q/A1m9+KxXB7SRz0K/| &&
      |4Nn04DiO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4ziO4z| &&
      |iO4zip5X/p5OFVxiRUDQAAAABJRU5ErkJggg==|.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = lv_imagebase64
      IMPORTING
        output = ev_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_filename = |image_{ iv_otype }_{ iv_objid }.png|.
      ev_mimetype = |image/png|.
    ENDIF.
  ENDMETHOD.


  METHOD enhance_entity_fieldchange.
    IF cs_entity IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_entity ASSIGNING FIELD-SYMBOL(<ls_changing_entity>).
      MOVE cs_entity TO <ls_changing_entity>.
    ENDIF.

    CHECK ct_entity IS NOT INITIAL.

    LOOP AT ct_entity ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity>-vis_updateable = <ls_entity>-editable.
      <ls_entity>-typename = 'Field Change Details'(tfd).

      DATA:
        lo_elem TYPE REF TO cl_abap_elemdescr.

      TRY.
          SELECT SINGLE otype INTO @DATA(lv_otype) FROM /sew/int_v_cun WHERE aend_id = @<ls_entity>-aend_id.
          IF lv_otype = 'P'.
            cl_abap_elemdescr=>describe_by_name( EXPORTING p_name = |PA{ <ls_entity>-infty }-{ <ls_entity>-field }|
              RECEIVING p_descr_ref = DATA(lo_tmp_pa) EXCEPTIONS type_not_found = 1 ).
            IF sy-subrc = 0.
              lo_elem ?= lo_tmp_pa.
            ENDIF.
          ELSEIF lv_otype = 'O'.
            cl_abap_elemdescr=>describe_by_name( EXPORTING p_name = |HRP{ <ls_entity>-infty }-{ <ls_entity>-field }|
              RECEIVING p_descr_ref = DATA(lo_tmp_om) EXCEPTIONS type_not_found = 1 ).
            IF sy-subrc = 0.
              lo_elem ?= lo_tmp_om.
            ENDIF.
          ENDIF.

          IF lo_elem IS NOT INITIAL.
            lo_elem->get_ddic_field( RECEIVING p_flddescr = DATA(lo_ddic_type)
              EXCEPTIONS no_ddic_type = 1 not_found = 2 OTHERS = 3 ).
            IF sy-subrc = 0.
              <ls_entity>-title = lo_ddic_type-fieldtext.
            ENDIF.
          ENDIF.
        CATCH cx_root.
          "NOOP
      ENDTRY.

      IF <ls_entity>-title IS INITIAL.
        <ls_entity>-title = '- No Field Name Found -'(nnf).
      ENDIF.

      <ls_entity>-imageurl = |/sap/opu/odata/SEW/HCM_INTCENTER_SRV/ImageSet(ObjectType='F',ObjectID='')/$value|.
    ENDLOOP.
  ENDMETHOD.


  METHOD enhance_entity_intrun.
    CHECK cs_entity IS NOT INITIAL.

    cs_entity-imageurl = |/sap/opu/odata/SEW/HCM_INTCENTER_SRV/ImageSet(ObjectType='{ cs_entity-otype }',ObjectID='{ cs_entity-sap_id }')/$value|.

* user selektieren
    TYPES: ty_objid_range TYPE RANGE OF objid.
    /mhp/smart_cl_repo_util=>get_orgview_assignments( EXPORTING iv_orgsel = '/SEW/INTCENT_US'
      IMPORTING et_keyobjects = DATA(lt_objectsel) ).
    DATA(lt_objsel) = VALUE ty_objid_range( FOR <ls_objectsel> IN lt_objectsel ( sign = 'I' option = 'EQ' low = <ls_objectsel>-realo ) ).

    SELECT SINGLE SUM( counter_chg ) AS counter_chg, SUM( counter_chg_e ) AS counter_chg_e
      FROM /sew/int_center_un INTO CORRESPONDING FIELDS OF @cs_entity WHERE int_run = @cs_entity-int_run
      AND sap_id = @cs_entity-sap_id AND cloud_id = @cs_entity-cloud_id AND sachb_sbgrpcode IN @lt_objsel
      GROUP BY int_run, sap_id, cloud_id.

    cs_entity-vis_criticality_counter_chg_e = COND #( WHEN cs_entity-counter_chg_e > 0 THEN 1 ELSE 3 ).
    cs_entity-counter_chg_ok = cs_entity-counter_chg - cs_entity-counter_chg_e.
    cs_entity-vis_criticality_counter_msg_e = COND #( WHEN cs_entity-counter_msg_e > 0 THEN 1 ELSE 3 ).
    cs_entity-counter_msg_ok = cs_entity-counter_msg - cs_entity-counter_msg_e.

    cs_entity-vis_hidden_msg = COND #( WHEN cs_entity-counter_msg = 0 THEN abap_true ELSE abap_false ).
    cs_entity-vis_hidden_om = COND #( WHEN cs_entity-otype = 'O' THEN abap_false ELSE abap_true ).
    cs_entity-vis_hidden_pa = COND #( WHEN cs_entity-otype = 'P' THEN abap_false ELSE abap_true ).

    IF cs_entity-sap_id IS NOT INITIAL.
      CASE cs_entity-otype.
        WHEN 'P'.
          SELECT SINGLE ename AS stext INTO CORRESPONDING FIELDS OF @cs_entity FROM pa0001 WHERE pernr = @cs_entity-sap_id
            AND begda <= @sy-datlo AND endda >= @sy-datlo.
        WHEN 'O'.
          SELECT SINGLE short, stext INTO CORRESPONDING FIELDS OF @cs_entity FROM hrp1000 WHERE objid = @cs_entity-sap_id
            AND otype = @cs_entity-otype AND begda <= @sy-datlo AND endda >= @sy-datlo.
      ENDCASE.
    ENDIF.

    IF cs_entity-stext IS INITIAL.
      cs_entity-short = 'N.N.'(nns).
      cs_entity-stext = '- No Name -'(nnl).
    ENDIF.

    SELECT SINGLE SUM( counter_msg ) AS counter FROM /sew/int_center_le INTO @DATA(lv_sum_status_03)
      WHERE int_run = @cs_entity-int_run AND sap_id = @cs_entity-sap_id AND cloud_id = @cs_entity-cloud_id
      AND status = '03' AND sachb_sbgrpcode IN @lt_objsel.

    SELECT SINGLE SUM( counter_msg ) AS counter FROM /sew/int_center_le INTO @DATA(lv_sum_status_01)
      WHERE int_run = @cs_entity-int_run AND sap_id = @cs_entity-sap_id AND cloud_id = @cs_entity-cloud_id
      AND status = '01' AND sachb_sbgrpcode IN @lt_objsel.

    SELECT SINGLE SUM( counter_msg ) AS counter FROM /sew/int_center_le INTO @DATA(lv_sum_status_07)
      WHERE int_run = @cs_entity-int_run AND sap_id = @cs_entity-sap_id AND cloud_id = @cs_entity-cloud_id
      AND status = '07' AND sachb_sbgrpcode IN @lt_objsel.

* not errors. Hide actions
    IF lv_sum_status_03 = 0.
      cs_entity-vis_hidden_action_done = cs_entity-vis_hidden_action_process = abap_true.
    ENDIF.

    cs_entity-vis_hidden_action_start = abap_true.
* no errors and some initial
    IF lv_sum_status_03 = 0 AND lv_sum_status_01 > 0.
      cs_entity-vis_hidden_action_start = abap_false.
    ENDIF.

    IF lv_sum_status_03 = 0 AND lv_sum_status_01 = 0 AND lv_sum_status_07 > 0.
      cs_entity-vis_hidden_action_process = abap_false.
    ENDIF.

    cs_entity-typename = 'Integration Run'(tir).
  ENDMETHOD.


  METHOD enhance_entity_it_aend.
    IF cs_entity IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_entity ASSIGNING FIELD-SYMBOL(<ls_changing_entity>).
      MOVE cs_entity TO <ls_changing_entity>.
    ENDIF.

    DATA:
      lt_range_infty TYPE RANGE OF infty,
      lt_range_subty TYPE RANGE OF subty,
      lt_idd07v      TYPE TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = '/SEW/DD_STATUS_DOM'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    lt_range_infty = VALUE #( FOR <ls_entities> IN ct_entity ( sign = 'I' option = 'EQ' low = <ls_entities>-infty ) ).
    SORT lt_range_infty BY low. DELETE ADJACENT DUPLICATES FROM lt_range_infty COMPARING low.
    lt_range_subty = VALUE #( FOR <ls_entities> IN ct_entity WHERE ( subty IS NOT INITIAL ) ( sign = 'I' option = 'EQ' low = <ls_entities>-subty ) ).
    SORT lt_range_subty BY low. DELETE ADJACENT DUPLICATES FROM lt_range_subty COMPARING low.

    IF lt_range_infty IS NOT INITIAL.
      SELECT infty, itext FROM t582s INTO TABLE @DATA(lt_infty_text) WHERE sprsl = @sy-langu AND infty IN @lt_range_infty AND itbld = ''.
      IF lt_range_subty IS NOT INITIAL.
        SELECT infty, subty, stext FROM t591s INTO TABLE @DATA(lt_subty_text) WHERE sprsl = @sy-langu AND infty IN @lt_range_infty AND subty IN @lt_range_subty.
      ENDIF.
    ENDIF.

    LOOP AT ct_entity ASSIGNING FIELD-SYMBOL(<ls_entity>).
      CASE <ls_entity>-status.
        WHEN '03'.
          <ls_entity>-vis_criticality_status = 1.
        WHEN '01' OR '07' OR '06' OR '05'.
          <ls_entity>-vis_criticality_status = 2.
        WHEN '02'.
          <ls_entity>-vis_criticality_status = 3.
      ENDCASE.

      <ls_entity>-vis_infty_text = VALUE #( lt_infty_text[ infty = <ls_entity>-infty ]-itext DEFAULT '- No Infotype Text -' ).
      IF <ls_entity>-subty IS NOT INITIAL.
        <ls_entity>-vis_subty_text = VALUE #( lt_subty_text[ infty = <ls_entity>-infty subty = <ls_entity>-subty ]-stext DEFAULT '- No Subtype Text -' ).
      ENDIF.

      <ls_entity>-vis_status_text = VALUE #( lt_idd07v[ valpos = <ls_entity>-status ]-ddtext DEFAULT '- No Status Text -'(nst) ).
    ENDLOOP.

    IF <ls_changing_entity> IS ASSIGNED.
      MOVE <ls_changing_entity> TO cs_entity.

      cs_entity-imageurl = |/sap/opu/odata/SEW/HCM_INTCENTER_SRV/ImageSet(ObjectType='I',ObjectID='{ cs_entity-infty }')/$value|.

* user selektieren
      TYPES: ty_objid_range TYPE RANGE OF objid.
      /mhp/smart_cl_repo_util=>get_orgview_assignments( EXPORTING iv_orgsel = '/SEW/INTCENT_US'
        IMPORTING et_keyobjects = DATA(lt_objectsel) ).
      DATA(lt_objsel) = VALUE ty_objid_range( FOR <ls_objectsel> IN lt_objectsel ( sign = 'I' option = 'EQ' low = <ls_objectsel>-realo ) ).

      SELECT SINGLE SUM( counter_msg ) AS counter_msg, SUM( counter_msg_e ) AS counter_msg_e FROM /sew/int_center_le
        INTO CORRESPONDING FIELDS OF @cs_entity WHERE int_run = @cs_entity-int_run AND aend_id = @cs_entity-aend_id
        AND sap_id = @cs_entity-pernr AND cloud_id = @cs_entity-cloud_id AND sachb_sbgrpcode IN @lt_objsel
        GROUP BY int_run, sap_id, cloud_id, aend_id .

      cs_entity-counter_msg_ok = cs_entity-counter_msg - cs_entity-counter_msg_e.
      cs_entity-vis_criticality_counter_msg_e = COND #( WHEN cs_entity-counter_msg_e > 0 THEN 1 ELSE 0 ).

      cs_entity-vis_hidden_msg = COND #( WHEN cs_entity-counter_msg = 0 THEN abap_true ELSE abap_false ).
      cs_entity-vis_hidden_subty = COND #( WHEN cs_entity-subty IS INITIAL THEN abap_true ELSE abap_false ).

      IF cs_entity-status = '03'.
        SELECT COUNT( * ) FROM /sew/int_msg_f INTO @DATA(lv_editable_count) WHERE aend_id = @cs_entity-aend_id
          AND editable = @abap_true.

        cs_entity-vis_updateable = COND #( WHEN lv_editable_count > 0 THEN abap_true ELSE abap_false ).
      ENDIF.

      cs_entity-typename = 'Infotype Change Details'(tid).
    ENDIF.
  ENDMETHOD.


  METHOD enhance_entity_message.
    IF cs_entity IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_entity ASSIGNING FIELD-SYMBOL(<ls_changing_entity>).
      MOVE cs_entity TO <ls_changing_entity>.
    ENDIF.

    DATA:
      lt_idd07v TYPE TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = '/SEW/DD_MSG_STATUS_DOM'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    LOOP AT ct_entity ASSIGNING FIELD-SYMBOL(<ls_entity>).
      CASE <ls_entity>-type.
        WHEN 'E' OR 'A'.
          <ls_entity>-vis_criticality_type = 1.
        WHEN 'W'.
          <ls_entity>-vis_criticality_type = 2.
        WHEN 'S'.
          <ls_entity>-vis_criticality_type = 3.
      ENDCASE.

      CASE <ls_entity>-status.
        WHEN '01'.
          <ls_entity>-vis_criticality_status = 1.
        WHEN '02'.
          <ls_entity>-vis_criticality_status = 3.
        WHEN '02'.
          <ls_entity>-vis_criticality_status = 2.
      ENDCASE.
      <ls_entity>-vis_status_text = VALUE #( lt_idd07v[ valpos = <ls_entity>-status ]-ddtext DEFAULT '- No Status Text -'(nst) ).
      DATA: note_nr TYPE msgno.
      note_nr = <ls_entity>-num.
      SHIFT note_nr LEFT DELETING LEADING '0'.
      SELECT SINGLE molga FROM /sew/int_center_le INTO @DATA(lv_molga) WHERE aend_id = @<ls_entity>-aend_id AND msg_id = @<ls_entity>-msg_guid.
      SELECT SINGLE msgtext FROM /sew/int_notes INTO @<ls_entity>-solution_description WHERE molga = @lv_molga AND
        msgid = @<ls_entity>-id AND msgno = @note_nr AND msgty = @<ls_entity>-type.
      IF <ls_entity>-solution_description IS INITIAL.
        SELECT SINGLE msgtext FROM /sew/int_notes INTO @<ls_entity>-solution_description WHERE molga = '*' AND
          msgid = @<ls_entity>-id AND msgno = @note_nr AND msgty = @<ls_entity>-type.
      ENDIF.

      <ls_entity>-vis_hidden_solution = COND #( WHEN <ls_entity>-solution_description IS INITIAL THEN abap_true ELSE abap_false ).

      <ls_entity>-typename = 'Message Details'(tmd).
    ENDLOOP.

    IF <ls_changing_entity> IS ASSIGNED.
      MOVE <ls_changing_entity> TO cs_entity.

      TYPES: ty_objid_range TYPE RANGE OF objid.
      /mhp/smart_cl_repo_util=>get_orgview_assignments( EXPORTING iv_orgsel = '/SEW/INTCENT_US'
        IMPORTING et_keyobjects = DATA(lt_objectsel) ).
      DATA(lt_objsel) = VALUE ty_objid_range( FOR <ls_objectsel> IN lt_objectsel ( sign = 'I' option = 'EQ' low = <ls_objectsel>-realo ) ).

      SELECT SINGLE int_run, SUM( counter_msg ) AS counter FROM /sew/int_center_le INTO @DATA(ls_sum)
        WHERE aend_id = @cs_entity-aend_id AND status = '03' AND sachb_sbgrpcode IN @lt_objsel GROUP BY int_run.

      IF sy-subrc = 0.
        SELECT COUNT( * ) FROM /sew/int_msg_f INTO @DATA(lv_editable_count) WHERE aend_id = @cs_entity-aend_id
          AND editable = @abap_true.

        cs_entity-vis_updateable = COND #( WHEN lv_editable_count > 0 THEN abap_true ELSE abap_false ).
      ENDIF.

      cs_entity-imageurl = |/sap/opu/odata/SEW/HCM_INTCENTER_SRV/ImageSet(ObjectType='M',ObjectID='{ cs_entity-type }')/$value|.
    ENDIF.
  ENDMETHOD.


  METHOD enhance_entity_om_aend.
    IF cs_entity IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_entity ASSIGNING FIELD-SYMBOL(<ls_changing_entity>).
      MOVE cs_entity TO <ls_changing_entity>.
    ENDIF.

    DATA:
      lt_range_infty TYPE RANGE OF infty,
      lt_range_subty TYPE RANGE OF subty,
      lt_idd07v      TYPE TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = '/SEW/DD_STATUS_DOM'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    lt_range_infty = VALUE #( FOR <ls_entities> IN ct_entity ( sign = 'I' option = 'EQ' low = <ls_entities>-infty ) ).
    SORT lt_range_infty BY low. DELETE ADJACENT DUPLICATES FROM lt_range_infty COMPARING low.
    lt_range_subty = VALUE #( FOR <ls_entities> IN ct_entity WHERE ( subty IS NOT INITIAL ) ( sign = 'I' option = 'EQ' low = <ls_entities>-subty ) ).
    SORT lt_range_subty BY low. DELETE ADJACENT DUPLICATES FROM lt_range_subty COMPARING low.

    IF lt_range_infty IS NOT INITIAL.
      SELECT infty, itext FROM t777t INTO TABLE @DATA(lt_infty_text) WHERE langu = @sy-langu AND infty IN @lt_range_infty.
      IF lt_range_subty IS NOT INITIAL.
        SELECT infty, subty, sutxt FROM t777u INTO TABLE @DATA(lt_subty_text) WHERE langu = @sy-langu AND infty IN @lt_range_infty AND subty IN @lt_range_subty.
      ENDIF.
    ENDIF.

    LOOP AT ct_entity ASSIGNING FIELD-SYMBOL(<ls_entity>).
      CASE <ls_entity>-status.
        WHEN '03'.
          <ls_entity>-vis_criticality_status = 1.
        WHEN '01' OR '07' OR '06' OR '05'.
          <ls_entity>-vis_criticality_status = 2.
        WHEN '02'.
          <ls_entity>-vis_criticality_status = 3.
      ENDCASE.

      <ls_entity>-vis_infty_text = VALUE #( lt_infty_text[ infty = <ls_entity>-infty ]-itext DEFAULT '- No Infotype Text -' ).
      IF <ls_entity>-subty IS NOT INITIAL.
        <ls_entity>-vis_subty_text = VALUE #( lt_subty_text[ infty = <ls_entity>-infty subty = <ls_entity>-subty ]-sutxt DEFAULT '- No Subtype Text -' ).
      ENDIF.

      <ls_entity>-vis_status_text = VALUE #( lt_idd07v[ valpos = <ls_entity>-status ]-ddtext DEFAULT '- No Status Text -'(nst) ).
    ENDLOOP.

    IF <ls_changing_entity> IS ASSIGNED.
      MOVE <ls_changing_entity> TO cs_entity.

      cs_entity-imageurl = |/sap/opu/odata/SEW/HCM_INTCENTER_SRV/ImageSet(ObjectType='O',ObjectID='{ cs_entity-sap_id }')/$value|.


      TYPES: ty_objid_range TYPE RANGE OF objid.
      /mhp/smart_cl_repo_util=>get_orgview_assignments( EXPORTING iv_orgsel = '/SEW/INTCENT_US'
        IMPORTING et_keyobjects = DATA(lt_objectsel) ).
      DATA(lt_objsel) = VALUE ty_objid_range( FOR <ls_objectsel> IN lt_objectsel ( sign = 'I' option = 'EQ' low = <ls_objectsel>-realo ) ).

      SELECT SINGLE SUM( counter_msg ) AS counter_msg, SUM( counter_msg_e ) AS counter_msg_e FROM /sew/int_center_le
        INTO CORRESPONDING FIELDS OF @cs_entity WHERE int_run = @cs_entity-int_run AND aend_id = @cs_entity-aend_id
        AND sap_id = @cs_entity-sap_id AND cloud_id = @cs_entity-cloud_id AND sachb_sbgrpcode IN @lt_objsel
        GROUP BY int_run, sap_id, cloud_id, aend_id .

      cs_entity-counter_msg_ok = cs_entity-counter_msg - cs_entity-counter_msg_e.
      cs_entity-vis_criticality_counter_msg_e = COND #( WHEN cs_entity-counter_msg_e > 0 THEN 1 ELSE 0 ).

      cs_entity-vis_hidden_msg = COND #( WHEN cs_entity-counter_msg = 0 THEN abap_true ELSE abap_false ).
      cs_entity-vis_hidden_subty = COND #( WHEN cs_entity-subty IS INITIAL THEN abap_true ELSE abap_false ).

      IF cs_entity-status = '03'.
        SELECT COUNT( * ) FROM /sew/int_msg_f INTO @DATA(lv_editable_count) WHERE aend_id = @cs_entity-aend_id
          AND editable = @abap_true.

        cs_entity-vis_updateable = COND #( WHEN lv_editable_count > 0 THEN abap_true ELSE abap_false ).
      ENDIF.

      cs_entity-typename = 'Organizational Change Details'(tod).
    ENDIF.
  ENDMETHOD.


  METHOD fieldchangeset_get_entity.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* daten lesen
    SELECT SINGLE * FROM /sew/int_msg_f INTO CORRESPONDING FIELDS OF @er_entity WHERE aend_id = @ls_full_key-aend_id
      AND field_id = @ls_full_key-field_id.

* daten vervollständigen
    IF sy-subrc = 0.
      enhance_entity_fieldchange( CHANGING cs_entity = er_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD fieldchangeset_get_entityset.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitysetcontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* kopiere pernr in sap id wenn diese gegeben ist (durch navigation über IT AEND
    IF ls_full_key-pernr IS NOT INITIAL.
      ls_full_key-sap_id = ls_full_key-pernr.
    ENDIF.

* daten lesen
    SELECT * FROM /sew/int_msg_f INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE aend_id = @ls_full_key-aend_id.


* daten vervollständigen
    enhance_entity_fieldchange( CHANGING ct_entity = et_entityset ).

* tech context anwenden
    /mhp/smart_cl_gw_helper=>apply_context_oninternaltable( EXPORTING io_entitysetcontext = io_tech_request_context
      CHANGING ct_table = et_entityset cv_maxcount = lv_count ).
    es_response_context-count = es_response_context-inlinecount = lv_count.
  ENDMETHOD.


  METHOD fieldchangeset_update_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

    er_entity-aedtm = sy-datlo.
    er_entity-uname = sy-uname.

    DATA(ls_fields) = CORRESPONDING /sew/int_msg_f( er_entity ).

* daten lesen
    SELECT SINGLE int_run, pernr AS sap_id, cloud_id, molga FROM /sew/int_it_aend INTO @DATA(ls_int_run)
      WHERE aend_id = @ls_fields-aend_id.
    IF sy-subrc <> 0.
      SELECT SINGLE int_run, sap_id, cloud_id, molga FROM /sew/int_om_aend INTO @ls_int_run
        WHERE aend_id = @ls_fields-aend_id.
    ENDIF.

    IF ls_int_run IS NOT INITIAL.
      DATA(lo_instance) = /sew/cl_int_status_handler=>get_instance( EXPORTING cloud_id = CONV #( ls_int_run-cloud_id )
        sap_id = CONV #( ls_int_run-sap_id ) int_run = ls_int_run-int_run molga = ls_int_run-molga ).
      lo_instance->set_field_change( field_id = ls_fields-field_id value_old = ls_fields-old_val value_new = ls_fields-changed_val
        aend_id = ls_fields-aend_id ).

      IF lo_instance->new_log_messages IS NOT INITIAL.
        DATA(lo_ex) = NEW /iwbep/cx_mgw_busi_exception( entity_type = iv_entity_name ).
        lo_ex->get_msg_container( )->add_messages_from_bapi( value #( for <ls_log_message> in lo_instance->new_log_messages
          ( type = <ls_log_message>-type id = <ls_log_message>-id number = <ls_log_message>-num  message = <ls_log_message>-message
            message_v1 = <ls_log_message>-message_v1 message_v2 = <ls_log_message>-message_v2  message_v3 = <ls_log_message>-message_v3
            message_v4 = <ls_log_message>-message_v4 ) ) ).
        RAISE EXCEPTION lo_ex.
      ENDIF.

      lo_instance->persist_data( source = /sew/cl_int_constants=>cockpit ).
    ENDIF.
  ENDMETHOD.


  METHOD infotypechangese_get_entity.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* daten lesen
    SELECT SINGLE * FROM /sew/int_it_aend INTO CORRESPONDING FIELDS OF @er_entity WHERE int_run = @ls_full_key-int_run
      AND cloud_id = @ls_full_key-cloud_id AND pernr = @ls_full_key-pernr AND aend_id = @ls_full_key-aend_id.

* daten vervollständigen
    IF sy-subrc = 0.
      enhance_entity_it_aend( CHANGING cs_entity = er_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD infotypechangese_get_entityset.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitysetcontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* kopiere pernr in sap id wenn diese gegeben ist (durch navigation über IT AEND
    IF ls_full_key-pernr IS NOT INITIAL.
      ls_full_key-sap_id = ls_full_key-pernr.
    ENDIF.

* daten lesen
    IF ls_full_key-aend_id IS INITIAL.
      SELECT * FROM /sew/int_it_aend INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE int_run = @ls_full_key-int_run
        AND cloud_pernr = @ls_full_key-cloud_id AND pernr = @ls_full_key-sap_id.
    ELSE.
      SELECT * FROM /sew/int_it_aend INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE int_run = @ls_full_key-int_run
        AND cloud_pernr = @ls_full_key-cloud_id AND pernr = @ls_full_key-sap_id AND aend_id = @ls_full_key-aend_id.
    ENDIF.

* daten vervollständigen
    enhance_entity_it_aend( CHANGING ct_entity = et_entityset ).

* tech context anwenden
    /mhp/smart_cl_gw_helper=>apply_context_oninternaltable( EXPORTING io_entitysetcontext = io_tech_request_context
      CHANGING ct_table = et_entityset cv_maxcount = lv_count ).
    es_response_context-count = es_response_context-inlinecount = lv_count.
  ENDMETHOD.


  METHOD integrationrunse_get_entity.
* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context
      IMPORTING et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab
      CHANGING cs_any = er_entity ).

* user selektieren
    TYPES: ty_objid_range TYPE RANGE OF objid.
    /mhp/smart_cl_repo_util=>get_orgview_assignments( EXPORTING iv_orgsel = '/SEW/INTCENT_US'
      IMPORTING et_keyobjects = DATA(lt_objectsel) ).
    DATA(lt_objsel) = VALUE ty_objid_range( FOR <ls_objectsel> IN lt_objectsel ( sign = 'I' option = 'EQ' low = <ls_objectsel>-realo ) ).

* daten lesen
    SELECT SINGLE int_run, sap_id, cloud_id, otype, stream_direction, MIN( timestamp ) AS timestamp, SUM( counter_msg ) AS counter_msg,
      SUM( counter_msg_e ) AS counter_msg_e FROM /sew/int_center_le INTO CORRESPONDING FIELDS OF @er_entity WHERE int_run = @er_entity-int_run
      AND sap_id = @er_entity-sap_id AND cloud_id = @er_entity-cloud_id AND sachb_sbgrpcode IN @lt_objsel
      GROUP BY int_run, sap_id, cloud_id, otype, stream_direction.

* daten anreichern
    enhance_entity_intrun( CHANGING cs_entity = er_entity ).
  ENDMETHOD.


  METHOD integrationrunse_get_entityset.
    "Reduce List Read to Dev User
    IF sy-uname = 'DEKUCCHR'.
      SELECT  int_run, sap_id, cloud_id, otype, stream_direction,
        SUM( counter_msg ) AS counter FROM /sew/int_center_le INTO CORRESPONDING FIELDS OF TABLE @et_entityset
        GROUP BY int_run, sap_id, cloud_id, otype, stream_direction.
    ENDIF.
  ENDMETHOD.


  METHOD messageset_get_entity.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* daten lesen
    SELECT SINGLE * FROM /sew/int_msg_l INTO CORRESPONDING FIELDS OF @er_entity WHERE aend_id = @ls_full_key-aend_id
      AND msg_guid = @ls_full_key-msg_guid.

* daten vervollständigen
    IF sy-subrc = 0.
      enhance_entity_message( CHANGING cs_entity = er_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD messageset_get_entityset.
    DATA:
      lt_range_guid32 TYPE RANGE OF guid_32,
      ls_full_key     TYPE ty_full_key,
      lv_count        TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitysetcontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* kopiere pernr in sap id wenn diese gegeben ist (durch navigation über IT AEND
    IF ls_full_key-pernr IS NOT INITIAL.
      ls_full_key-sap_id = ls_full_key-pernr.
    ENDIF.

* daten lesen
    IF ls_full_key-field_id IS NOT INITIAL.
      SELECT 'I' AS sign, 'EQ' AS option, msg_guid AS low FROM /sew/int_msg_f INTO TABLE @lt_range_guid32
        WHERE aend_id = @ls_full_key-aend_id AND field_id = @ls_full_key-field_id.
      IF lt_range_guid32 IS NOT INITIAL.
        SELECT * FROM /sew/int_msg_l INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE msg_guid IN @lt_range_guid32.
      ENDIF.
    ELSEIF ls_full_key-msg_guid IS NOT INITIAL.
      SELECT * FROM /sew/int_msg_l INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE msg_guid = @ls_full_key-msg_guid.
    ELSEIF ls_full_key-aend_id IS NOT INITIAL.
      SELECT * FROM /sew/int_msg_l INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE aend_id = @ls_full_key-aend_id.
    ELSEIF ls_full_key-int_run IS NOT INITIAL AND ( ls_full_key-sap_id IS NOT INITIAL OR ls_full_key-cloud_id IS NOT INITIAL ).
      SELECT 'I' AS sign, 'EQ' AS option, aend_id AS low FROM /sew/int_center_un INTO TABLE @lt_range_guid32
        WHERE int_run = @ls_full_key-int_run AND sap_id = @ls_full_key-sap_id AND cloud_id = @ls_full_key-cloud_id.
      IF lt_range_guid32 IS NOT INITIAL.
        SELECT * FROM /sew/int_msg_l INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE aend_id IN @lt_range_guid32.
      ENDIF.
    ENDIF.

* daten vervollständigen
    enhance_entity_message( CHANGING ct_entity = et_entityset ).

* tech context anwenden
    /mhp/smart_cl_gw_helper=>apply_context_oninternaltable( EXPORTING io_entitysetcontext = io_tech_request_context
      CHANGING ct_table = et_entityset cv_maxcount = lv_count ).
    es_response_context-count = es_response_context-inlinecount = lv_count.
  ENDMETHOD.


  METHOD orgchangeset_get_entity.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitycontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* daten lesen
    SELECT SINGLE * FROM /sew/int_om_aend INTO CORRESPONDING FIELDS OF @er_entity WHERE int_run = @ls_full_key-int_run
      AND cloud_id = @ls_full_key-cloud_id AND sap_id = @ls_full_key-sap_id AND aend_id = @ls_full_key-aend_id.

* daten vervollständigen
    IF sy-subrc = 0.
      enhance_entity_om_aend( CHANGING cs_entity = er_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD orgchangeset_get_entityset.
    DATA:
      ls_full_key TYPE ty_full_key,
      lv_count    TYPE int4.

* schlüsseldaten lesen
    /mhp/smart_cl_gw_helper=>get_lastnaventity( EXPORTING io_entitysetcontext = io_tech_request_context IMPORTING
      et_key_tab = DATA(lt_key_tab) ev_entityset_name = DATA(lv_entityset) ev_sourceentity_name = DATA(lv_source_entityset) ).
    /mhp/smart_cl_gw_helper=>read_strucdata_from_keytab( EXPORTING it_key_tab = lt_key_tab CHANGING cs_any = ls_full_key ).

* kopiere pernr in sap id wenn diese gegeben ist (durch navigation über IT AEND
    IF ls_full_key-pernr IS NOT INITIAL.
      ls_full_key-sap_id = ls_full_key-pernr.
    ENDIF.

* daten lesen
    IF ls_full_key-aend_id IS INITIAL.
      SELECT * FROM /sew/int_om_aend INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE int_run = @ls_full_key-int_run
        AND cloud_id = @ls_full_key-cloud_id AND sap_id = @ls_full_key-sap_id.
    ELSE.
      SELECT * FROM /sew/int_om_aend INTO CORRESPONDING FIELDS OF TABLE @et_entityset WHERE int_run = @ls_full_key-int_run
        AND cloud_id = @ls_full_key-cloud_id AND sap_id = @ls_full_key-sap_id AND aend_id = @ls_full_key-aend_id.
    ENDIF.

* daten vervollständigen
    enhance_entity_om_aend( CHANGING ct_entity = et_entityset ).

* tech context anwenden
    /mhp/smart_cl_gw_helper=>apply_context_oninternaltable( EXPORTING io_entitysetcontext = io_tech_request_context
      CHANGING ct_table = et_entityset cv_maxcount = lv_count ).
    es_response_context-count = es_response_context-inlinecount = lv_count.
  ENDMETHOD.
ENDCLASS.
