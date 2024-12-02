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
  
  # Simulation logic
  for (i in 1:n_events) {
    next_available_time <- min(active_servers)
    free_server <- which(active_servers == next_available_time)[1]  
    start_service[i] <- max(arrivals[i], next_available_time)
    waiting_time[i] <- start_service[i] - arrivals[i]
    finish_service[i] <- start_service[i] + service_times[i]
    active_servers[free_server] <- finish_service[i]
    queue_lengths[i] <- sum(arrivals < start_service[i] & finish_service > arrivals)
  }
  
  total_service_time <- sum(service_times)
  total_time <- max(finish_service) - min(arrivals)
  
  # 1. Utilization
  lambda <- 1 / mean_arrival
  mu <- 1 / mean_service
  U <- lambda / (servers * mu)
  
  P <- sum(waiting_time > 0) / n_events
  
  # 3. Average waiting time in the queue (Wq)
  W_q <- P / (servers * mu * (1 - U))
  
  # 4. Average number of calls in the queue (Lq)
  L_q <- lambda * W_q
  
  list(
    metrics = list(
      utilization = U,
      avg_wait_time = W_q,
      avg_calls_on_queue = L_q
    ),
    events = data.frame(
      Arrival = arrivals,
      StartService = start_service,
      FinishService = finish_service,
      WaitingTime = waiting_time,
      ServiceTime = service_times,
      QueueLength = queue_lengths
    )
  )
}
