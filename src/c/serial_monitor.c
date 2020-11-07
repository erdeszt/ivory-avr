#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>

/** Opens a serial connection to an arduino device.
 * The connection is duplex and asynchronous with no tty
 *
 * @param device_name The name of the device
 * @param baud_rate   Baud rate of the communication
 * @return The file descriptor or -1 on error.
 */
const int32_t serial_open(const uint8_t *device_name, const speed_t baud_rate)
{
    assert(NULL != device_name);

    /** Open the device
     * Options are:
     *   O_RDWR   - duplex
     *   O_NCTTY  - no tty(so an accidental ctrl-c doesn't drop the line)
     *   O_NDELAY - no delay, read returns 0 when no input is ready
     */
    int32_t device = open(device_name, O_RDWR | O_NOCTTY | O_NDELAY);

    if (device < 0) {
        perror("open_port: Unable to open port");
        return -1;
    }

    fcntl(device, F_SETFL, 0);

    /** Read the current options */
    struct termios options;

    tcgetattr(device, &options);

    assert(NULL != &options);

    /** Set the baud rate on both directions */
    cfsetispeed(&options, baud_rate);
    cfsetospeed(&options, baud_rate);

    /** Enable the receiver and set local mode */
    options.c_cflag |= (CLOCAL | CREAD);

    /** Set the new attributes, immediately */
    tcsetattr(device, TCSANOW, &options);

    return device;
}

/** Close a serial device
 *
 * @param device The file descriptor of the device
 * @return 0 on success -1 on error.
 */
const int32_t serial_close(const int32_t device)
{
    assert(device >= 0);

    return close(device);
}

/** Echos a serial input to a given stream
 * Runs in an infinite loop so blocks the current thread.
 * It's intended for debugging purposes.
 * Example: serial_echo(device, stdout);
 *
 * @param device The file desriptor of the device
 * @param out    The output stream
 * @return       The function doesn't return under normal circumstances
 *               If it returns it indicates and error and the value is always -1
 */
const int32_t serial_echo(const int32_t device, FILE *out)
{
    /* We read 1 character at a time */
    const size_t buffer_size = 1;
    const useconds_t wait = 1000;
    uint8_t buff[buffer_size];

    while (1) {
        ssize_t bytes_read = read(device, buff, buffer_size);

        if (bytes_read < 0) {
            perror("serial_echo: Could not read");
            break;
        }

        /* No input is ready, we wait */
        if (bytes_read == 0) {
            usleep(wait);
        }
        /* Echo input to the given stream */
        else {
            fputc(buff[0], out);
        }
    }

    return -1;
}

/** Reads bytes until it matches the given one
 * Terminates the buffer with a null byte so the maximum number of bytes
 * read from the device is max_size - 1
 *
 * @param device    The file descriptor of the device
 * @param buffer    The buffer to read to
 * @param term_char Terminating character
 * @param max_size  Maximum number of bytes to read
 */
const ssize_t serial_read_until( const int32_t device
                     , uint8_t *buffer
                     , const uint8_t term_char
                     , const size_t max_size)
{
    assert(NULL != buffer);

    uint8_t read_buffer[1] = { 0 };
    ssize_t bytes_read;
    ssize_t total_bytes_read = 0;

    memset((void *)buffer, '\0', max_size);

    /* Reading until we reach the limit or found the terminator character */
    while (total_bytes_read < max_size - 1) {

        bytes_read = read(device, read_buffer, 1);

        if (bytes_read < 0) {
            perror("serial_read_until: Could not read");
            break;
        }

        /* If there's no input wait 1ms and try again */
        if (0 == bytes_read) {
            usleep(1000);
            continue;
        }
        /* If we've found the terminating character
         * return the number of bytes read */
        else if (term_char == read_buffer[0]) {
            assert('\0' == buffer[total_bytes_read]);

            return total_bytes_read;
        }
        /* Put the new character in the buffer and increment the index */
        else {
            buffer[total_bytes_read] = read_buffer[0];
            total_bytes_read++;
        }
    }

    assert(term_char != buffer[total_bytes_read]);
    assert('\0' == buffer[max_size]);

    /* If we could not find the terminating character or
     * there was a read error indicate it.
     */
    return -1;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: serialmonitor COMX\n");
        return 1;
    }

    const size_t BUFFER_SIZE = 1024;
    uint8_t buffer[BUFFER_SIZE];
    uint8_t* port = (uint8_t*)argv[1];
    speed_t speed = 9600;

    int32_t serial = serial_open(argv[1], speed);

    if (serial == -1) {
        printf("Could not open %s\n", port);
        return 2;
    }

    serial_echo(serial, stdout);

    return 0;
}