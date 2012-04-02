#!/usr/bin/env ruby
workspaces = []
`wmctrl -d`.each_line do |line|
  columns = line.split
  if columns[7] == "N/A"
    workspace_name = columns[8..-1].join(" ")
  else
    workspace_name = columns[9..-1].join(" ")
  end

  unless workspace_name == "NSP"
    workspaces << (columns.first + "/" + workspace_name)
  end
end

print workspaces.join("\0")
