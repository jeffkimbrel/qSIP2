# works as expected

    Code
      qsip_data(example_source_object, example_sample_object, example_feature_object)
    Message
      There are 15 source_mat_ids, and they are all shared between the source and sample objects
      There are 284 sample_ids, and they are all shared between the sample and feature objects
    Output
      <qsip_data>
       @ source_data          : <qsip_source_data>
       .. @ data           : tibble [15 x 6] (S3: tbl_df/tbl/data.frame)
       $ isotope           : chr [1:15] "12C" "12C" "12C" "12C" ...
       $ isotopolog        : chr [1:15] "glucose" "glucose" "glucose" "glucose" ...
       $ source_mat_id     : chr [1:15] "S149" "S150" "S151" "S152" ...
       $ total_copies_per_g: num [1:15] 34838665 53528072 95774992 9126192 41744046 ...
       $ total_dna         : num [1:15] 74.5 109 182.2 23.7 67.6 ...
       $ Moisture          : chr [1:15] "Normal" "Normal" "Normal" "Normal" ...
       .. @ isotope        : chr "Isotope"
       .. @ isotopolog     : chr "isotopolog"
       .. @ source_mat_id  : chr "source"
       .. @ time           : chr "NULL"
       .. @ total_abundance: chr "NULL"
       @ sample_data          : <qsip_sample_data>
       .. @ data                : tibble [284 x 7] (S3: tbl_df/tbl/data.frame)
       $ sample_id           : chr [1:284] "149_F1" "149_F2" "149_F3" "149_F4" ...
       $ source_mat_id       : chr [1:284] "S149" "S149" "S149" "S149" ...
       $ gradient_position   : int [1:284] 1 2 3 4 5 6 7 8 9 10 ...
       $ gradient_pos_density: num [1:284] 1.78 1.77 1.77 1.76 1.75 ...
       $ gradient_pos_amt    : num [1:284] 4474 987 4003 3960 5726 ...
       $ gradient_pos_rel_amt: num [1:284] 1.28e-04 2.83e-05 1.15e-04 1.14e-04 1.64e-04 ...
       $ dna_conc            : num [1:284] 0 0 0 0 0.00124 ...
       .. @ sample_id           : chr "sample"
       .. @ source_mat_id       : chr "source"
       .. @ gradient_position   : chr "Fraction"
       .. @ gradient_pos_density: chr "density_g_ml"
       .. @ gradient_pos_amt    : chr "avg_16S_g_soil"
       .. @ gradient_pos_rel_amt: chr "gradient_pos_rel_amt"
       @ feature_data         : <qsip_feature_data>
       .. @ data      : tibble [2,030 x 285] (S3: tbl_df/tbl/data.frame)
       $ feature_id: chr [1:2030] "ASV_1" "ASV_2" "ASV_3" "ASV_4" ...
       $ 149_F1    : num [1:2030] 1245 1471 342 288 317 ...
       $ 149_F2    : num [1:2030] 376 569 152 119 108 73 82 47 74 58 ...
       $ 149_F3    : num [1:2030] 582 830 211 161 95 130 102 52 89 73 ...
       $ 149_F4    : num [1:2030] 1258 1373 389 294 292 ...
       $ 149_F5    : num [1:2030] 692 737 218 157 164 112 136 90 83 64 ...
       $ 149_F6    : num [1:2030] 1482 1777 461 381 310 ...
       $ 149_F7    : num [1:2030] 812 2086 304 242 216 ...
       $ 149_F8    : num [1:2030] 743 2603 293 219 188 ...
       $ 149_F9    : num [1:2030] 285 3375 134 135 83 ...
       $ 149_F10   : num [1:2030] 47 1145 30 27 27 ...
       $ 149_F11   : num [1:2030] 265 2236 187 147 80 ...
       $ 149_F12   : num [1:2030] 434 982 221 215 110 85 86 79 171 94 ...
       $ 149_F13   : num [1:2030] 773 436 446 378 303 141 34 26 153 72 ...
       $ 149_F14   : num [1:2030] 4695 829 1702 1520 1237 ...
       $ 149_F15   : num [1:2030] 3967 918 1065 844 795 ...
       $ 149_F16   : num [1:2030] 2295 1077 721 561 549 ...
       $ 149_F17   : num [1:2030] 1411 974 536 498 413 ...
       $ 149_F18   : num [1:2030] 780 584 335 223 244 121 36 34 93 56 ...
       $ 149_F19   : num [1:2030] 2000 1626 836 734 646 ...
       $ 150_F1    : num [1:2030] 197 2006 111 81 79 ...
       $ 150_F2    : num [1:2030] 131 1445 76 85 53 ...
       $ 150_F3    : num [1:2030] 177 1854 113 110 71 ...
       $ 150_F4    : num [1:2030] 384 1844 163 118 105 ...
       $ 150_F5    : num [1:2030] 543 5403 340 327 211 ...
       $ 150_F6    : num [1:2030] 591 5568 343 344 261 ...
       $ 150_F7    : num [1:2030] 472 5450 345 291 205 97 143 159 222 158 ...
       $ 150_F8    : num [1:2030] 536 6331 384 332 236 ...
       $ 150_F9    : num [1:2030] 330 4722 277 227 151 ...
       $ 150_F10   : num [1:2030] 409 5425 317 295 184 ...
       $ 150_F11   : num [1:2030] 313 3498 248 202 137 ...
       $ 150_F12   : num [1:2030] 229 1762 149 148 84 ...
       $ 150_F13   : num [1:2030] 616 2329 460 397 249 ...
       $ 150_F14   : num [1:2030] 1339 2197 715 728 406 ...
       $ 150_F15   : num [1:2030] 1986 1647 787 724 541 ...
       $ 150_F16   : num [1:2030] 2581 1996 1003 897 661 ...
       $ 150_F17   : num [1:2030] 1869 1970 757 679 468 ...
       $ 150_F18   : num [1:2030] 1069 1384 494 436 304 ...
       $ 150_F19   : num [1:2030] 1627 2266 706 595 486 ...
       $ 151_F1    : num [1:2030] 392 1920 104 94 103 0 70 59 81 89 ...
       $ 151_F2    : num [1:2030] 300 1581 121 76 95 ...
       $ 151_F3    : num [1:2030] 140 1539 64 44 26 ...
       $ 151_F4    : num [1:2030] 250 1681 92 92 62 ...
       $ 151_F5    : num [1:2030] 232 1953 113 107 99 ...
       $ 151_F6    : num [1:2030] 220 1612 85 86 95 ...
       $ 151_F7    : num [1:2030] 292 3057 89 108 93 ...
       $ 151_F8    : num [1:2030] 256 4118 117 120 86 ...
       $ 151_F9    : num [1:2030] 179 3199 67 59 56 ...
       $ 151_F10   : num [1:2030] 142 3265 75 51 53 ...
       $ 151_F11   : num [1:2030] 56 1393 0 42 30 ...
       $ 151_F12   : num [1:2030] 102 1280 65 53 38 0 48 44 94 53 ...
       $ 151_F13   : num [1:2030] 462 1588 290 258 172 ...
       $ 151_F14   : num [1:2030] 1462 1001 618 511 401 ...
       $ 151_F15   : num [1:2030] 2670 819 636 594 699 390 26 26 136 80 ...
       $ 151_F16   : num [1:2030] 3248 1404 747 576 498 ...
       $ 151_F17   : num [1:2030] 1070 736 292 201 258 165 22 34 77 49 ...
       $ 151_F18   : num [1:2030] 1154 1249 361 325 335 ...
       $ 151_F19   : num [1:2030] 846 1084 288 222 237 ...
       $ 152_F1    : num [1:2030] 505 259 78 67 92 73 40 27 57 49 ...
       $ 152_F2    : num [1:2030] 1258 1063 248 232 381 ...
       $ 152_F3    : num [1:2030] 1964 1848 437 388 379 ...
       $ 152_F4    : num [1:2030] 2236 2235 439 424 494 ...
       $ 152_F5    : num [1:2030] 2223 3934 525 467 561 ...
       $ 152_F6    : num [1:2030] 2921 4021 691 494 814 ...
       $ 152_F7    : num [1:2030] 1187 1133 182 203 270 ...
       $ 152_F8    : num [1:2030] 1236 1993 191 267 336 ...
       $ 152_F9    : num [1:2030] 507 1611 92 107 114 ...
       $ 152_F10   : num [1:2030] 345 4983 112 90 127 ...
       $ 152_F11   : num [1:2030] 248 5557 111 76 83 ...
       $ 152_F12   : num [1:2030] 330 5867 158 120 152 ...
       $ 152_F13   : num [1:2030] 1083 5397 427 384 498 ...
       $ 152_F14   : num [1:2030] 7262 6213 1988 1983 2577 ...
       $ 152_F15   : num [1:2030] 11167 2384 1984 1833 3347 ...
       $ 152_F16   : num [1:2030] 7663 1320 1036 893 2256 ...
       $ 152_F17   : num [1:2030] 3701 844 495 397 1022 ...
       $ 152_F18   : num [1:2030] 2590 2182 489 429 792 ...
       $ 152_F19   : num [1:2030] 3171 3549 658 574 969 ...
       $ 161_F1    : num [1:2030] 752 887 208 175 135 132 168 74 81 57 ...
       $ 161_F2    : num [1:2030] 458 469 106 122 132 118 93 45 45 51 ...
       $ 161_F3    : num [1:2030] 1530 1710 447 330 297 210 277 147 125 127 ...
       $ 161_F4    : num [1:2030] 1790 1348 678 762 835 ...
       $ 161_F5    : num [1:2030] 1262 1607 309 245 319 ...
       $ 161_F6    : num [1:2030] 1056 1237 254 266 258 ...
       $ 161_F7    : num [1:2030] 306 618 92 82 82 66 96 34 62 44 ...
       $ 161_F8    : num [1:2030] 586 622 181 144 144 90 76 62 101 42 ...
       $ 161_F9    : num [1:2030] 497 2950 159 128 93 87 329 200 88 84 ...
       $ 161_F10   : num [1:2030] 260 2219 105 102 78 ...
       $ 161_F11   : num [1:2030] 316 4221 166 168 136 ...
       $ 161_F12   : num [1:2030] 628 3171 382 363 252 ...
       $ 161_F13   : num [1:2030] 2514 2370 1206 1179 710 ...
       $ 161_F14   : num [1:2030] 4013 1308 1252 1162 992 ...
       $ 161_F15   : num [1:2030] 2254 593 594 473 455 ...
       $ 161_F16   : num [1:2030] 4370 1733 896 827 920 ...
       $ 161_F17   : num [1:2030] 1025 886 268 240 228 ...
       $ 161_F18   : num [1:2030] 1855 1789 499 503 524 ...
       $ 161_F19   : num [1:2030] 2467 3394 754 622 629 ...
       $ 162_F1    : num [1:2030] 272 1115 137 126 92 ...
       $ 162_F2    : num [1:2030] 214 355 64 86 85 44 578 160 82 32 ...
       $ 162_F3    : num [1:2030] 529 920 239 217 216 ...
        [list output truncated]
       .. @ feature_id: chr "ASV"
       .. @ taxonomy  :'data.frame':	0 obs. of  0 variables
       .. @ type      : chr "counts"
       @ shared               :List of 2
       .. $ source_mat_ids:List of 3
       ..  ..$ shared     : chr [1:15] "S149" "S150" "S151" "S152" ...
       ..  ..$ source_data: NULL
       ..  ..$ sample_data: NULL
       .. $ sample_ids    :List of 3
       ..  ..$ shared      : chr [1:284] "149_F1" "149_F2" "149_F3" "149_F4" ...
       ..  ..$ sample_data : NULL
       ..  ..$ feature_data: NULL
       @ tube_rel_abundance   : tibble [39,183 x 6] (S3: tbl_df/tbl/data.frame)
       $ feature_id          : chr [1:39183] "ASV_1" "ASV_1" "ASV_1" "ASV_1" ...
       $ sample_id           : chr [1:39183] "149_F1" "149_F2" "149_F3" "149_F4" ...
       $ source_mat_id       : chr [1:39183] "S149" "S149" "S149" "S149" ...
       $ tube_rel_abundance  : num [1:39183] 1.80e-05 4.11e-06 1.60e-05 1.67e-05 2.82e-05 ...
       $ gradient_pos_density: num [1:39183] 1.78 1.77 1.77 1.76 1.75 ...
       $ gradient_pos_rel_amt: num [1:39183] 1.28e-04 2.83e-05 1.15e-04 1.14e-04 1.64e-04 ...
       @ wads                 : tibble [9,282 x 4] (S3: tbl_df/tbl/data.frame)
       $ feature_id   : chr [1:9282] "ASV_1000" "ASV_1001" "ASV_1001" "ASV_1001" ...
       $ source_mat_id: chr [1:9282] "S203" "S149" "S164" "S179" ...
       $ WAD          : num [1:9282] 1.71 1.75 1.76 1.69 1.73 ...
       $ n_fractions  : int [1:9282] 1 1 1 1 1 1 1 1 1 1 ...
       @ source_wads          : tibble [15 x 2] (S3: tbl_df/tbl/data.frame)
       $ source_mat_id: chr [1:15] "S149" "S150" "S151" "S152" ...
       $ WAD          : num [1:15] 1.71 1.71 1.71 1.71 1.71 ...
       @ fraction_counts      : tibble [30,450 x 3] (S3: tbl_df/tbl/data.frame)
       $ feature_id   : chr [1:30450] "ASV_1" "ASV_1" "ASV_1" "ASV_1" ...
       $ source_mat_id: chr [1:30450] "S203" "S149" "S164" "S179" ...
       $ n_fractions  : int [1:30450] 19 19 19 16 19 19 18 19 15 19 ...
       @ filtered_feature_data:'data.frame':	0 obs. of  0 variables
       @ filtered_wad_data    :'data.frame':	0 obs. of  0 variables
       @ filter_results       : list()
       @ resamples            : list()
       @ EAF                  :'data.frame':	0 obs. of  0 variables
       @ growth               : list()
