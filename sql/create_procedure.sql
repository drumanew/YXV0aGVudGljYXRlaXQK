create procedure chain_of_custody ( IN prod_id VARCHAR( 16 ) )
  select ts, participant_id, action from assignment
  where product_id = prod_id
  order by ts, action
