capacity = read.csv("../data/02_input_capacity.csv", stringsAsFactors = T)
input_target = read.csv("../data/02_input_target.csv", stringsAsFactors = T)
arc_costs = read.csv("../data/02_03_input_shipmentsCost_example.csv", stringsAsFactors = T)

#capacity$Monthly.Capacity = capacity$Monthly.Capacity * 5
#write.csv(capacity, "../data/02_input_capacity.csv", row.names = T)

# Create empty transport data frame
transport = data.frame(
  Origin = character(),
  Destination = character(),
  Product = character(),
  Month = character(),
  Quantity = integer()
)

# Create empty production data frame
production = data.frame(
  Country = character(),
  Product = character(),
  Month = character(),
  Quantity = integer()
)

totalMonthlyProdutionSum = 0

# Sort arcs by cost
arc_costs = arc_costs[order(arc_costs$Unit.Cost),]

for (month in levels(input_target$Month)) {
  monthly_request = input_target[input_target$Month == month,]
  
  states_request = data.frame(
    Country = levels(input_target$Country),
    Quantity = sapply(
      levels(input_target$Country), 
      function(x) sum(monthly_request$Quantity[monthly_request$Country == x])),
    Capacity = sapply(
      levels(input_target$Country), 
      function(x) capacity$Monthly.Capacity[capacity$Country == x])
  )
  
  states_request$Remaining = states_request$Capacity - states_request$Quantity
  
  # Create empty dataset of transfers
  transport_partial = data.frame(
    Origin = character(),
    Destination = character(),
    Quantity = integer()
  )
  
  # For each arc in the sorted arc_costs, check if the origin and destination states have remaining capacity
  for (i in 1:nrow(arc_costs)) {
    origin = as.character(arc_costs$Origin[i])
    destination = as.character(arc_costs$Destination[i])
    
    # Check if the origin and destination states have remaining capacity
    if (states_request$Remaining[states_request$Country == origin] > 0 && 
        states_request$Remaining[states_request$Country == destination] < 0) {
      # Calculate the amount to transfer
      transfer_amount = min(
        states_request$Remaining[states_request$Country == origin], 
        -states_request$Remaining[states_request$Country == destination]
      )
      
      # Create a new row in the transport dataset
      transport_partial = rbind(
        transport_partial, 
        data.frame(
          Origin = origin,
          Destination = destination,
          Quantity = transfer_amount
        )
      )
      
      # Update the remaining capacity for both states
      states_request$Remaining[states_request$Country == origin] = 
        states_request$Remaining[states_request$Country == origin] - transfer_amount
      states_request$Remaining[states_request$Country == destination] = 
        states_request$Remaining[states_request$Country == destination] + transfer_amount
    }
  }
  
  # Lower total production if globally we are overproducting
  states_request$Production = states_request$Capacity - sapply(states_request$Remaining, FUN = function(x) max(x, 0))
  totalMonthlyProdutionSum = sum(states_request$Production) + totalMonthlyProdutionSum
  
  # Fill the production data frame by splitting states_request$Production by product
  for (i in 1:nrow(states_request)) {
    for (p in levels(monthly_request$Product)) {
      if (monthly_request$Quantity[monthly_request$Country == states_request$Country[i] & monthly_request$Product == p] == 0) {
        next
      }
      if (states_request$Production[i] > 0) {
        # Calculate the quantity for each product
        quantity = min(
          monthly_request$Quantity[monthly_request$Country == states_request$Country[i] & monthly_request$Product == p],
          states_request$Production[i]
        )
        production = rbind(
          production, 
          data.frame(
            Country = states_request$Country[i],
            Product = p,
            Month = month,
            Quantity = quantity
          )
        )
        # Update the production for the state
        states_request$Production[i] = states_request$Production[i] - quantity
        
        # Update the request for the product
        monthly_request$Quantity[monthly_request$Country == states_request$Country[i] & monthly_request$Product == p] = 
          monthly_request$Quantity[monthly_request$Country == states_request$Country[i] & monthly_request$Product == p] - quantity
      } else {
        break
      }
    }
  }
  
  # Fill the transport data frame by splitting transport_partial$Quantity by product
  for (i in 1:nrow(transport_partial)) {
    for (p in levels(monthly_request$Product)) {
      if (monthly_request$Quantity[
        monthly_request$Country == transport_partial$Destination[i] &
        monthly_request$Product == p] == 0) {
        next
      }
      if (transport_partial$Quantity[i] > 0) {
        # Calculate the quantity for each product
        quantity = min(
          monthly_request$Quantity[monthly_request$Country == transport_partial$Destination[i] & monthly_request$Product == p],
          transport_partial$Quantity[i]
        )
        transport = rbind(
          transport, 
          data.frame(
            Origin = transport_partial$Origin[i],
            Destination = transport_partial$Destination[i],
            Product = p,
            Month = month,
            Quantity = quantity
          )
        )
        
        # Add product to the origin state in the production data frame
        # If the product is not already in the production data frame, add it
        if (!any(production$Country == transport_partial$Origin[i] & production$Product == p & 
                  production$Month == month)) {
          production = rbind(
            production, 
            data.frame(
              Country = transport_partial$Origin[i],
              Product = p,
              Month = month,
              Quantity = 0
            )
          )
        }
        # Update the quantity for the origin state
        production$Quantity[production$Country == transport_partial$Origin[i] & 
                             production$Product == p & 
                             production$Month == month] = 
          production$Quantity[production$Country == transport_partial$Origin[i] & 
                             production$Product == p & 
                             production$Month == month] + quantity
        
        # Update the transfer for the state
        transport_partial$Quantity[i] = transport_partial$Quantity[i] - quantity
        
        # Update the request for the product
        monthly_request$Quantity[monthly_request$Country == transport_partial$Destination[i] & monthly_request$Product == p] = 
          monthly_request$Quantity[monthly_request$Country == transport_partial$Destination[i] & monthly_request$Product == p] - quantity
      } else {
        break
      }
    }
  }
  
  print(sum(monthly_request$Quantity))
}

print(totalMonthlyProdutionSum)
print(sum(production$Quantity))
print(sum(input_target$Quantity))

write.csv(production, "03_output_productionPlan_1522.csv", row.names = T)
write.csv(transport, "03_output_shipments_1522.csv", row.names = T)
