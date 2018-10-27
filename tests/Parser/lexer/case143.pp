$sources_list_content = $_purge['sources.list'] ? {
  true    => "# Repos managed by puppet.\n",
  default => undef,
}

