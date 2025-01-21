def docker-nuke [] {
    print "Warning: This will remove all Docker containers, images, volumes, and networks!"
    let confirm = input "Are you sure you want to proceed? (yes/no): "

    if $confirm == "yes" {
        # Stop all running containers
        print "Stopping all running containers..."
        docker ps -q | lines | each {|id| docker stop $id } | ignore

        # Remove all containers with retries
        print "Removing all containers..."
        let container_ids = (docker ps -aq | lines)
        while ($container_ids | length) > 0 {
            $container_ids | each {|id| 
                if (docker rm -f $id | complete).exit_code != 0 {
                    print $"Failed to remove container ($id), retrying..."
                    sleep 1sec
                }
            }
            let container_ids = (docker ps -aq | lines)
        }

        # Remove all images with retry limit
        print "Removing all images..."
        mut failed_images = []
        let image_ids = (docker images -q | lines)

        for $id in $image_ids {
            mut attempts = 0
            const max_attempts = 3
            while $attempts < $max_attempts {
                if (docker rmi -f $id | complete).exit_code == 0 {
                    break
                } else {
                    print $"Failed to remove image ($id), attempt ($attempts + 1)/($max_attempts)..."
                    sleep 1sec
                    $attempts += 1
                }
            }
            if $attempts == $max_attempts {
                $failed_images = ($failed_images | append $id)
            }
        }

        # Remove all volumes
        print "Removing all volumes..."
        docker volume ls -q | lines | each {|id| docker volume rm $id | ignore }

        # Remove all networks except the default ones
        print "Removing all networks..."
        docker network ls -q | lines | each {|id| docker network rm $id | ignore }

        # Final prune
        print "Pruning system to remove dangling resources..."
        docker system prune -af --volumes | ignore

        # Report any failures
        if ($failed_images | length) > 0 {
            print "Some images could not be removed after 5 attempts:"
            $failed_images | each {|id| print $"- Image ID: ($id)" }
        } else {
            print "All images removed successfully."
        }

        print "Docker environment is now clean."
    } else {
        print "Operation cancelled."
    }
}
