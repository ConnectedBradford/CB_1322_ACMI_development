# Build a SNOMED to AC drug look up from the AC_medications_codes_v2.csv file.

# Olly Butters
# 2/8/22

# We have a wide format table with a column for each AC with a 0/1 in each
# cell to indicate if that AC is present in the medication. There are some medications
# which have multiple AC components in them.
# We need to map SNOMED codes of the medications people are taking to the AC components
# so we need 
# SNOMED code, AC component
# with multiple rows per drug that has multiple AC components.

raw <- read.csv("AC_medication_codes_v2.csv")

ac_component = colnames(raw)[8:117]
ac_component
length(ac_component)

out <- data.frame()

# Cycle through all the rows in the file
for (this_row in 1:nrow(raw)){
  for (this_ac_component in ac_component) {
    if(raw[this_row, this_ac_component] == 1) {
      out <- rbind(out, cbind(raw[this_row, 1:6], AC = this_ac_component))
    }
  }
}

write.csv(out,"SNOMED_to_AC_lookup.csv", row.names = FALSE)
