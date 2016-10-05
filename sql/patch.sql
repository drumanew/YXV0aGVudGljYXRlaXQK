update product
  set current_owner_id = (
    select participant_id from assignment
    where product_id = product.id
    order by ts desc
    limit 1
  )
