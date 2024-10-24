# works correctly

    Code
      qsip_sample_data(add_taxonomy_testdf, sample_id = "sample", source_mat_id = "source",
        gradient_position = "Fraction", gradient_pos_density = "density_g_ml",
        gradient_pos_amt = "avg_16S_g_soil", gradient_pos_rel_amt = "gradient_pos_rel_amt")
    Output
      <qSIP2::qsip_sample_data>
      source_material_id count: 15
      sample_id count: 284

# no gradient_pos_rel_amt given will autocalculate

    Code
      qsip_sample_data(example_sample_df, sample_id = "sample", source_mat_id = "source",
        gradient_position = "Fraction", gradient_pos_density = "density_g_ml",
        gradient_pos_amt = "avg_16S_g_soil")
    Message
      <gradient_pos_rel_amt> not specified. Calculating using avg_16S_g_soil column
    Output
      <qSIP2::qsip_sample_data>
      source_material_id count: 15
      sample_id count: 284

