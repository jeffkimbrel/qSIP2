# works as expected

    Code
      find_shared_ids(example_source_object, example_sample_object,
        example_feature_object)
    Message
      There are 15 source_mat_ids, and they are all shared between the source and sample objects
      There are 284 sample_ids, and they are all shared between the sample and feature objects
    Output
      $source_mat_ids
      $source_mat_ids$shared
       [1] "S149" "S150" "S151" "S152" "S161" "S162" "S163" "S164" "S178" "S179"
      [11] "S180" "S200" "S201" "S202" "S203"
      
      $source_mat_ids$source_data
      NULL
      
      $source_mat_ids$sample_data
      NULL
      
      
      $sample_ids
      $sample_ids$shared
        [1] "149_F1"  "149_F2"  "149_F3"  "149_F4"  "149_F5"  "149_F6"  "149_F7" 
        [8] "149_F8"  "149_F9"  "149_F10" "149_F11" "149_F12" "149_F13" "149_F14"
       [15] "149_F15" "149_F16" "149_F17" "149_F18" "149_F19" "150_F1"  "150_F2" 
       [22] "150_F3"  "150_F4"  "150_F5"  "150_F6"  "150_F7"  "150_F8"  "150_F9" 
       [29] "150_F10" "150_F11" "150_F12" "150_F13" "150_F14" "150_F15" "150_F16"
       [36] "150_F17" "150_F18" "150_F19" "151_F1"  "151_F2"  "151_F3"  "151_F4" 
       [43] "151_F5"  "151_F6"  "151_F7"  "151_F8"  "151_F9"  "151_F10" "151_F11"
       [50] "151_F12" "151_F13" "151_F14" "151_F15" "151_F16" "151_F17" "151_F18"
       [57] "151_F19" "152_F1"  "152_F2"  "152_F3"  "152_F4"  "152_F5"  "152_F6" 
       [64] "152_F7"  "152_F8"  "152_F9"  "152_F10" "152_F11" "152_F12" "152_F13"
       [71] "152_F14" "152_F15" "152_F16" "152_F17" "152_F18" "152_F19" "161_F1" 
       [78] "161_F2"  "161_F3"  "161_F4"  "161_F5"  "161_F6"  "161_F7"  "161_F8" 
       [85] "161_F9"  "161_F10" "161_F11" "161_F12" "161_F13" "161_F14" "161_F15"
       [92] "161_F16" "161_F17" "161_F18" "161_F19" "162_F1"  "162_F2"  "162_F3" 
       [99] "162_F4"  "162_F5"  "162_F6"  "162_F7"  "162_F8"  "162_F9"  "162_F10"
      [106] "162_F11" "162_F12" "162_F13" "162_F14" "162_F15" "162_F16" "162_F17"
      [113] "162_F18" "163_F1"  "163_F2"  "163_F3"  "163_F4"  "163_F5"  "163_F6" 
      [120] "163_F7"  "163_F8"  "163_F9"  "163_F10" "163_F11" "163_F12" "163_F13"
      [127] "163_F14" "163_F15" "163_F16" "163_F17" "163_F18" "163_F19" "164_F1" 
      [134] "164_F2"  "164_F3"  "164_F4"  "164_F5"  "164_F6"  "164_F7"  "164_F8" 
      [141] "164_F9"  "164_F10" "164_F11" "164_F12" "164_F13" "164_F14" "164_F15"
      [148] "164_F16" "164_F17" "164_F18" "164_F19" "178_F1"  "178_F2"  "178_F3" 
      [155] "178_F4"  "178_F5"  "178_F6"  "178_F7"  "178_F8"  "178_F9"  "178_F10"
      [162] "178_F11" "178_F12" "178_F13" "178_F14" "178_F15" "178_F16" "178_F17"
      [169] "178_F18" "179_F1"  "179_F2"  "179_F3"  "179_F4"  "179_F5"  "179_F6" 
      [176] "179_F7"  "179_F8"  "179_F9"  "179_F10" "179_F11" "179_F12" "179_F13"
      [183] "179_F14" "179_F15" "179_F16" "179_F17" "179_F18" "179_F19" "179_F20"
      [190] "180_F1"  "180_F2"  "180_F3"  "180_F4"  "180_F5"  "180_F6"  "180_F7" 
      [197] "180_F8"  "180_F9"  "180_F10" "180_F11" "180_F12" "180_F13" "180_F14"
      [204] "180_F15" "180_F16" "180_F17" "180_F18" "180_F19" "200_F2"  "200_F1" 
      [211] "200_F3"  "200_F4"  "200_F5"  "200_F6"  "200_F7"  "200_F8"  "200_F9" 
      [218] "200_F10" "200_F11" "200_F12" "200_F13" "200_F14" "200_F15" "200_F16"
      [225] "200_F17" "200_F18" "200_F19" "201_F1"  "201_F2"  "201_F3"  "201_F4" 
      [232] "201_F5"  "201_F6"  "201_F7"  "201_F8"  "201_F9"  "201_F10" "201_F11"
      [239] "201_F12" "201_F13" "201_F14" "201_F15" "201_F16" "201_F17" "201_F18"
      [246] "201_F19" "202_F1"  "202_F2"  "202_F3"  "202_F4"  "202_F5"  "202_F6" 
      [253] "202_F7"  "202_F8"  "202_F9"  "202_F10" "202_F11" "202_F12" "202_F13"
      [260] "202_F14" "202_F15" "202_F16" "202_F17" "202_F18" "202_F19" "203_F1" 
      [267] "203_F2"  "203_F3"  "203_F4"  "203_F5"  "203_F6"  "203_F7"  "203_F8" 
      [274] "203_F9"  "203_F10" "203_F11" "203_F12" "203_F13" "203_F14" "203_F15"
      [281] "203_F16" "203_F17" "203_F18" "203_F19"
      
      $sample_ids$sample_data
      NULL
      
      $sample_ids$feature_data
      NULL
      
      
