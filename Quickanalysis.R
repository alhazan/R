#Quick analysis

cant_users = length(unique(datos$userid))

comp_users = datos[, list(":="(count = .N), , by = country]



