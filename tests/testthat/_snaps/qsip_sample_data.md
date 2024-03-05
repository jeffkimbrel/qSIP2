# works correctly

    Code
      qsip_sample_data(qsip_sample_data_test_df, sample_id = "sample", source_mat_id = "source",
        gradient_position = "Fraction", gradient_pos_density = "density_g_ml",
        gradient_pos_amt = "avg_16S_g_soil", gradient_pos_rel_amt = "gradient_pos_rel_amt")
    Output
      <qsip_sample_data>
       @ data                : tibble [284 x 7] (S3: tbl_df/tbl/data.frame)
       $ sample_id           : chr [1:284] "149_F1" "149_F2" "149_F3" "149_F4" ...
       $ source_mat_id       : chr [1:284] "S149" "S149" "S149" "S149" ...
       $ gradient_position   : int [1:284] 1 2 3 4 5 6 7 8 9 10 ...
       $ gradient_pos_density: num [1:284] 1.78 1.77 1.77 1.76 1.75 ...
       $ gradient_pos_amt    : num [1:284] 4474 987 4003 3960 5726 ...
       $ gradient_pos_rel_amt: num [1:284] 1.28e-04 2.83e-05 1.15e-04 1.14e-04 1.64e-04 ...
       $ dna_conc            : num [1:284] 0 0 0 0 0.00124 ...
       @ sample_id           : chr "sample"
       @ source_mat_id       : chr "source"
       @ gradient_position   : chr "Fraction"
       @ gradient_pos_density: chr "density_g_ml"
       @ gradient_pos_amt    : chr "avg_16S_g_soil"
       @ gradient_pos_rel_amt: chr "gradient_pos_rel_amt"

# no gradient_pos_rel_amt given will autocalculate

    Code
      qsip_sample_data(qsip_sample_data_test_df_no_rel, sample_id = "sample",
        source_mat_id = "source", gradient_position = "Fraction",
        gradient_pos_density = "density_g_ml", gradient_pos_amt = "avg_16S_g_soil")
    Message <simpleMessage>
      <gradient_pos_rel_amt> not specified. Calculating using avg_16S_g_soil column
    Output
      <qsip_sample_data>
       @ data                : tibble [284 x 7] (S3: tbl_df/tbl/data.frame)
       $ sample_id           : chr [1:284] "149_F1" "149_F2" "149_F3" "149_F4" ...
       $ source_mat_id       : chr [1:284] "S149" "S149" "S149" "S149" ...
       $ gradient_position   : int [1:284] 1 2 3 4 5 6 7 8 9 10 ...
       $ gradient_pos_density: num [1:284] 1.78 1.77 1.77 1.76 1.75 ...
       $ gradient_pos_amt    : num [1:284] 4474 987 4003 3960 5726 ...
       $ gradient_pos_rel_amt: num [1:284] 1.28e-04 2.83e-05 1.15e-04 1.14e-04 1.64e-04 ...
       $ dna_conc            : num [1:284] 0 0 0 0 0.00124 ...
       @ sample_id           : chr "sample"
       @ source_mat_id       : chr "source"
       @ gradient_position   : chr "Fraction"
       @ gradient_pos_density: chr "density_g_ml"
       @ gradient_pos_amt    : chr "avg_16S_g_soil"
       @ gradient_pos_rel_amt: chr "gradient_pos_rel_amt"

