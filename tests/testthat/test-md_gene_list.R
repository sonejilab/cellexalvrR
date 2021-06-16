context('md_gene_list')

genes = c('x', 'a', 'b', 'c')

text = md_gene_links( genes )

exp = '<details>\r\n  <summary>Click to expand gene list</summary>\r\n\r\n[x](https://www.genecards.org/cgi-bin/carddisp.pl?gene=x) \n [a](https://www.genecards.org/cgi-bin/carddisp.pl?gene=a) \n [b](https://www.genecards.org/cgi-bin/carddisp.pl?gene=b) \n [c](https://www.genecards.org/cgi-bin/carddisp.pl?gene=c) \n\r\n</details>\r\n\r\n'

expect_equal( text, exp, label?"expected gene list")

