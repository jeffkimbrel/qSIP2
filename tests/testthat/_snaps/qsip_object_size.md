# size of list looks OK

    Code
      qsip_object_size(l)
    Output
        $group   size
      1      A 8.7 Mb
      2      B 3.9 Mb

# size of single object looks OK

    Code
      qsip_object_size(example_qsip_object)
    Output
                         @slot      size
      1           feature_data    4.7 Mb
      2     tube_rel_abundance    1.9 Mb
      3        fraction_counts  716.3 Kb
      4                   wads  375.5 Kb
      5            source_data  320.6 Kb
      6            sample_data  246.3 Kb
      7                 shared     20 Kb
      8            source_wads      2 Kb
      9  filtered_feature_data 600 bytes
      10     filtered_wad_data 600 bytes
      11                   EAF 600 bytes
      12        filter_results  48 bytes
      13             resamples  48 bytes
      14                growth  48 bytes

