#!/usr/bin/env ruby
workspaces = []
`wmctrl -d`.each_line do |line|
  columns = line.split
  if columns[7] == "N/A"
    workspace_name = columns[8..-1].join(" ")
  else
    workspace_name = columns[9..-1].join(" ")
  end

  if workspace_name == ARGV[0].strip
    system "wmctrl -s #{columns.first}"
    break
  end
end
