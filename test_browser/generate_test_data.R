# generate_test_data.R
# Generate Japanese test CSV files in UTF-8 and Shift-JIS (CP932) encodings.
# These files are used for browser-based character encoding and SEM tests.

df <- data.frame(
  国語 = c(80, 90, 50, 60, 70, 85, 45, 65, 75, 95),
  数学 = c(75, 95, 45, 70, 60, 80, 40, 60, 70, 90),
  英語 = c(85, 90, 60, 65, 75, 80, 50, 70, 80, 100)
)

# Output files in the workspace root (parent of test_browser/)
# When running Rscript from the workspace root, relative path is fine.

# Save as UTF-8
con_utf8 <- file("test_utf8.csv", open = "w", encoding = "UTF-8")
write.csv(df, con_utf8, row.names = FALSE)
close(con_utf8)
cat("Generated test_utf8.csv successfully.\n")

# Save as Shift-JIS (CP932)
con_sjis <- file("test_sjis.csv", open = "w", encoding = "CP932")
write.csv(df, con_sjis, row.names = FALSE)
close(con_sjis)
cat("Generated test_sjis.csv successfully.\n")
