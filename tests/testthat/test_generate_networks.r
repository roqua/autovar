context('generate_networks')

test_that('new_net_cfg() defines net_cfg correctly', {
  net_cfg <- new_net_cfg()
  expect_equal(class(net_cfg),'net_cfg')
})

test_that('generate_networks accepts valid requests only',{
  expect_match(generate_networks(c('a','b')),
               'is not a data.frame')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 6),
               'Timestamp argument is not a character')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 'not a timestamp'),
               'Wrong timestamp format')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 pairs='single'),
               'pairs should have even length')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 measurements_per_day=17),
               'measurements_per_day')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 measurements_per_day=0),
               'measurements_per_day')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 measurements_per_day=-5),
               'measurements_per_day')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 max_network_size=1),
               'max_network_size')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 max_network_size=7),
               'max_network_size')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 always_include='nonexisting'),
               'always include var nonexisting not found')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 always_include='id',
                                 pairs=c('id','tijdstip')),
               'always include var id also found in pairs')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 pairs=c('id','nonexisting')),
               'pair var nonexisting not found')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 pairs=c('id','id')),
               'variable id cannot pair with itself')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 positive_variables=c('id','nonexisting')),
               'positive variable nonexisting not found')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 negative_variables=c('id','nonexisting')),
               'negative variable nonexisting not found')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 negative_variables=c('tijdstip','id'),
                                 positive_variables=c('id','something')),
               'variable id cannot be both')
  expect_match(generate_networks(generate_numerical_test_data(5),
                                 '2014-05-06',
                                 labels=list(tijdstip='Tijdstip',nonexisting='Nonexisting')),
               'trying to specify label for unknown variable nonexisting')
})
