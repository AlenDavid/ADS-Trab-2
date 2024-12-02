simulate_queue <- function(mean_arrival, sd_arrival, mean_service, sd_service, n_events, servers) {
  # Parameters:
  # mean_arrival: Mean of inter-arrival times
  # sd_arrival: Standard deviation of inter-arrival times
  # mean_service: Mean of service times
  # sd_service: Standard deviation of service times
  # n_events: Number of events to simulate
  # servers: Number of available servers
  
  # Generate inter-arrival and service times using a normal distribution
  inter_arrival_times <- abs(rnorm(n_events, mean = mean_arrival, sd = sd_arrival))  
  service_times <- abs(rnorm(n_events, mean = mean_service, sd = sd_service)) 
  arrivals <- cumsum(inter_arrival_times)
  
  start_service <- numeric(n_events) 
  finish_service <- numeric(n_events) 
  waiting_time <- numeric(n_events) 
  active_servers <- rep(0, servers)
  queue_lengths <- numeric(n_events)
  server_states <- matrix(0, nrow = n_events, ncol = servers)  # Server states for each event
  
  # Simulation logic
  for (i in 1:n_events) {
    next_available_time <- min(active_servers)
    free_server <- which(active_servers == next_available_time)[1]  
    start_service[i] <- max(arrivals[i], next_available_time)
    waiting_time[i] <- start_service[i] - arrivals[i]
    finish_service[i] <- start_service[i] + service_times[i]
    active_servers[free_server] <- finish_service[i]
    server_states[i, ] <- active_servers  # Log server states
    queue_lengths[i] <- sum(arrivals < start_service[i] & finish_service > arrivals)
  }
  
  total_service_time <- sum(service_times)
  total_time <- max(finish_service) - min(arrivals)
  
  # Metrics
  lambda <- 1 / mean_arrival
  mu <- 1 / mean_service
  U <- lambda / (servers * mu)
  P <- sum(waiting_time > 0) / n_events
  W_q <- P / (servers * mu * (1 - U))
  L_q <- lambda * W_q
  
  list(
    metrics = list(
      utilization = U,
      avg_wait_time = W_q,
      avg_calls_on_queue = L_q
    ),
    events = data.frame(
      Iteration = 1:n_events,
      Arrival = arrivals,
      StartService = start_service,
      FinishService = finish_service,
      WaitingTime = waiting_time,
      ServiceTime = service_times,
      QueueLength = queue_lengths
    ),
    server_states = server_states
  )
}
