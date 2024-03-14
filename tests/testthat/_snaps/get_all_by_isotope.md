# correct input types work

    Code
      get_all_by_isotope(example_qsip_object, "12C")
    Output
      [1] "S149" "S150" "S151" "S152" "S161" "S162" "S163" "S164"

---

    Code
      get_all_by_isotope(example_source_object, "12C")
    Output
      [1] "S149" "S150" "S151" "S152" "S161" "S162" "S163" "S164"

---

    Code
      get_all_by_isotope(example_qsip_object, "13C")
    Output
      [1] "S178" "S179" "S180" "S200" "S201" "S202" "S203"

---

    Code
      get_all_by_isotope(example_source_object, "13C")
    Output
      [1] "S178" "S179" "S180" "S200" "S201" "S202" "S203"

---

    Code
      get_all_by_isotope(example_qsip_object, "labeled")
    Message <simpleMessage>
      WARNING: 15N not found in data
      WARNING: 18O not found in data
    Output
      [1] "S178" "S179" "S180" "S200" "S201" "S202" "S203"

---

    Code
      get_all_by_isotope(example_qsip_object, "unlabeled")
    Message <simpleMessage>
      WARNING: 14N not found in data
      WARNING: 16O not found in data
    Output
      [1] "S149" "S150" "S151" "S152" "S161" "S162" "S163" "S164"

# multiple isotopes work

    Code
      get_all_by_isotope(example_qsip_object, c("12C", "13C"))
    Output
       [1] "S149" "S150" "S151" "S152" "S161" "S162" "S163" "S164" "S178" "S179"
      [11] "S180" "S200" "S201" "S202" "S203"

