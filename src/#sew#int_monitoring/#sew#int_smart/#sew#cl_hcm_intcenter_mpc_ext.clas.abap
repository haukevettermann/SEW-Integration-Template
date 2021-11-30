class /SEW/CL_HCM_INTCENTER_MPC_EXT definition
  public
  inheriting from /SEW/CL_HCM_INTCENTER_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_HCM_INTCENTER_MPC_EXT IMPLEMENTATION.


  METHOD define.
    super->define( ).

* datums annotationen für dedizierte Datumsanzeige
    DATA:
      lt_entitiesets TYPE TABLE OF REF TO /iwbep/if_mgw_odata_entity_set,
      lt_entities    TYPE TABLE OF REF TO /iwbep/if_mgw_odata_entity_typ.

    APPEND model->get_entity_type( 'OrgChange' ) TO lt_entities.
    APPEND model->get_entity_type( 'InfotypeChange' ) TO lt_entities.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<lo_entity>).
      LOOP AT <lo_entity>->get_properties( ) ASSIGNING FIELD-SYMBOL(<lo_property>) WHERE name = 'AENDT' OR name = 'BEGDA'
          OR name = 'ENDDA'.
        DATA(lo_dateanno) = <lo_property>-property->/iwbep/if_mgw_odata_annotatabl~create_annotation( EXPORTING
            iv_annotation_namespace = /iwbep/if_mgw_med_odata_types=>gc_sap_namespace ).
        lo_dateanno->add( iv_key = 'display-format' iv_value = 'Date' ).
      ENDLOOP.
    ENDLOOP.

    APPEND model->get_entity_set( 'OrgChangeSet' ) TO lt_entitiesets.
    APPEND model->get_entity_set( 'InfotypeChangeSet' ) TO lt_entitiesets.
    APPEND model->get_entity_set( 'MessageSet' ) TO lt_entitiesets.
    APPEND model->get_entity_set( 'FieldChangeSet' ) TO lt_entitiesets.

    LOOP AT lt_entitiesets ASSIGNING FIELD-SYMBOL(<lo_entitiesets>).
      DATA(lr_updanno) = <lo_entitiesets>->create_annotation( 'sap' ).
      lr_updanno->add( iv_key = 'updatable-path' iv_value = 'VisUpdateable' ).
    ENDLOOP.

* mimetype für bilde
    model->get_entity_type( 'Image' )->get_property( 'MimeType' )->set_as_content_type( ).
  ENDMETHOD.
ENDCLASS.
