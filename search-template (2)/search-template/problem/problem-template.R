# =======================================================================
# Group Name:
# Students:
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(file, gustos_clientes) {
  # Crear el estado inicial como una lista vacía de ingredientes
  estado_inicial <- list(ingredientes = character())
  
  # Crear el estado objetivo como una lista de ingredientes que le gusten al menos al 30% de los clientes
  estado_objetivo <- list(ingredientes = list())
  
  # Obtener la cantidad de clientes
  num_clientes <- length(gustos_clientes)
  
  # Crear un vector con la cantidad de veces que aparece cada ingrediente en las listas de gustos de los clientes
  ingredientes_gustados <- table(unlist(lapply(gustos_clientes, function(x) x$gustos)))
  
  # Iterar sobre los ingredientes y agregar a la lista de estado objetivo los que aparecen en al menos el 30% de las listas de gustos
  for (ingrediente in names(ingredientes_gustados)) {
    if (ingredientes_gustados[ingrediente] / num_clientes >= 0.3) {
      estado_objetivo$ingredientes <- c(estado_objetivo$ingredientes, ingrediente)
    }
  }
  
  # Devolver el estado inicial y el estado objetivo como una lista
  return(list(estado_inicial = estado_inicial, estado_objetivo = estado_objetivo))
  
  #---------------------------------------------------------------------------------

  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  problem$name      <- paste0("OnePizza - [", file, "]")
  problem$size      <- as.numeric(unlist(read.csv(filename, header=FALSE, nrows=1)))
  # problem$state_initial     <- <INSERT CODE HERE>
  # problem$state_final       <- <INSERT CODE HERE>
  # problem$actions_possible  <- <INSERT CODE HERE>
  
  # You can add additional attributes
  # problem$<aditional_attribute>  <- <INSERT CODE HERE>
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
    # Verificar que el ingrediente no esté ya en la lista
    if (accion$ingrediente %in% estado$ingredientes) {
      return(FALSE)
    }
    
    # Verificar que el ingrediente no esté en la lista de disgustos de ningún cliente
    for (cliente in gustos_clientes) {
      if (accion$ingrediente %in% cliente$disgustos) {
        return(FALSE)
      }
    }
    
    # Si el ingrediente no está en la lista y no está en la lista de disgustos de ningún cliente, la acción es aplicable
    return(TRUE)
  }
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  # Crear una copia del estado actual
  nuevo_estado <- list(ingredientes = estado$ingredientes)
  
  # Agregar el nuevo ingrediente a la lista
  nuevo_estado$ingredientes <- c(nuevo_estado$ingredientes, accion$ingrediente)
  
  # Devolver el nuevo estado
  return(nuevo_estado)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  # Verificar que la lista de ingredientes del estado es igual o superior a la lista de ingredientes del estado objetivo
  if (length(estado$ingredientes) >= length(estado_objetivo$ingredientes)) {
    # Verificar que todos los ingredientes del estado objetivo están en la lista de ingredientes del estado actual
    for (ingrediente in estado_objetivo$ingredientes) {
      if (!(ingrediente %in% estado$ingredientes)) {
        return(FALSE)
      }
    }
    # Si la lista de ingredientes del estado actual es igual o superior a la lista de ingredientes del estado objetivo y todos los ingredientes del estado objetivo están en la lista de ingredientes del estado actual, el estado es final
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}