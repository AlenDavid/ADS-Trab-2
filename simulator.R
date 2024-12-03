simulate_queue <- function(mean_arrival, sd_arrival, mean_service, sd_service, n_events, servers) {
  inter_arrival_times <- abs(round(rnorm(n_events, mean = mean_arrival, sd = sd_arrival)))  
  service_times <- abs(round(rnorm(n_events, mean = mean_service, sd = sd_service))) 
  arrivals <- cumsum(inter_arrival_times)
  
  start_service <- numeric(n_events) 
  finish_service <- numeric(n_events) 
  waiting_time <- numeric(n_events) 
  active_servers <- rep(0, servers)
  queue_lengths <- numeric(n_events)

  for (i in 1:n_events) {
    next_available_time <- min(active_servers)
    free_server <- which(active_servers == next_available_time)[1]  
    start_service[i] <- max(arrivals[i], next_available_time)
    waiting_time[i] <- max(0, start_service[i] - arrivals[i]) 
    finish_service[i] <- start_service[i] + service_times[i]
    active_servers[free_server] <- finish_service[i]
    queue_lengths[i] <- sum(arrivals < start_service[i] & finish_service > arrivals)
  }

  total_service_time <- sum(service_times) 
  total_waiting_time <- sum(waiting_time) 
  total_arrival_time <- max(arrivals) 
  U <- total_service_time / (total_arrival_time * servers)
  W_q <- total_waiting_time / n_events l
  L_q <- total_waiting_time / total_service_time 
  
  #debugging
  cat("\n### Métricas Calculadas ###\n")
  cat(sprintf("Total Service Time: %.3f\n", total_service_time))
  cat(sprintf("Total Waiting Time: %.3f\n", total_waiting_time))
  cat(sprintf("Total Arrival Time: %.3f\n", total_arrival_time))
  cat(sprintf("Taxa de Utilização (U): %.3f\n", U))
  cat(sprintf("Tempo Médio de Espera (Wq): %.3f\n", W_q))
  cat(sprintf("Número Médio na Fila (Lq): %.3f\n", L_q))
  cat("\n")
  
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
    )
  )
}
