#!/usr/bin/env ruby
require "pp"

workspaces = {}
`wmctrl -d`.each_line do |line|
  columns = line.split
  if columns[7] == "N/A"
    workspace_name = columns[8..-1].join(" ")
  else
    workspace_name = columns[9..-1].join(" ")
  end

  workspaces[columns.first.to_i] = workspace_name
end

windows = []
`wmctrl -x -l`.each_line do |line|
  id, workspace_id, resource_class, host, *title = line.split

  title = title.join(" ")
  workspace = workspaces[workspace_id.to_i] || workspace_id
  win_class = resource_class.split(".").last

  unless win_class == "Gpicker" || workspace == "NSP"
    # replace /
    workspace.gsub!(/\//, " ")
    title.gsub!(/\//, " ")

    windows << "#{id}/#{win_class}/#{title}"
  end

end

print windows.join("\0")
