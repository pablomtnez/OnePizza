# Función para inicializar el problema

initialize.problem <- function(file) {
  
  # Lee el archivo de texto y lo guarda como un vector de caracteres
  lines <- readLines(file)
  
  # El primer elemento del vector indica la cantidad de clientes
  num_customers <- as.integer(lines[1])
  
  # Se crea un vector vacío para guardar los gustos de los clientes
  customers <- vector(mode = "list", length = num_customers)
  
  # Se recorren las líneas correspondientes a cada cliente
  for (i in 1:num_customers) {
    # Las líneas impares son los ingredientes que le gustan al cliente
    likes <- unlist(strsplit(lines[2*i], " "))
    # Las líneas pares son los ingredientes que no le gustan al cliente
    dislikes <- unlist(strsplit(lines[2*i+1], " "))
    # Se crea un vector con los gustos y disgustos del cliente
    customers[[i]] <- list(likes = likes, dislikes = dislikes)
  }
  
  # Se devuelve un objeto list que contiene los gustos de los clientes
  return(list(customers = customers))
}

# Función para verificar si una acción es aplicable a un estado
is.applicable <- function(state, action, problem) {
  # Se crea un vector con los ingredientes de la acción
  action_ingredients <- unlist(strsplit(action, " "))
  # Se recorre la lista de gustos de los clientes
  for (customer in problem$customers) {
    # Se verifica si el cliente le gusta al menos uno de los ingredientes de la acción
    if (length(intersect(customer$likes, action_ingredients)) > 0) {
      # Si al menos un cliente le gusta la acción, entonces es aplicable
      return(TRUE)
    }
  }
  # Si ninguna persona le gusta la acción, entonces no es aplicable
  return(FALSE)
}

# Función para aplicar una acción a un estado
effect <- function(state, action, problem) {
  # Se crea un vector con los ingredientes de la acción
  action_ingredients <- unlist(strsplit(action, " "))
  # Se crea un nuevo vector con los ingredientes que ya estaban en la pizza
  old_ingredients <- state$ingredients
  # Se agregan los nuevos ingredientes a la pizza
  new_ingredients <- unique(c(old_ingredients, action_ingredients))
  # Se crea un nuevo estado con los nuevos ingredientes
  new_state <- list(ingredients = new_ingredients)
  # Se devuelve el nuevo estado
  return(new_state)
}

# Función para verificar si un estado es final
is.final.state <- function(state, final_state, problem) {
  # Se calcula el número de clientes a los que les gusta la pizza
  num_liked_customers <- sum(sapply(problem$customers, function(x) {
    length(intersect(x$likes, state$ingredients)) == length(x$likes)
  }))
  # Se calcula el porcentaje de clientes satisfechos
  percent_liked_customers <- num_liked_customers / length(problem$customers)
  # Si el porcentaje de clientes satisfechos es mayor o igual que el 30%, entonces el estado es final
  if (percent_liked_customers >= 0.3) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
