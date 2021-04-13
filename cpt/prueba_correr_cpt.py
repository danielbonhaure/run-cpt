
import subprocess
import os
import sys
import signal
import select
import shlex


def run_command(command):
    process = subprocess.Popen(shlex.split(command),
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               preexec_fn=os.setsid, encoding='utf8')
    #
    contador, stdout_lines = 0, []
    #
    epoll = select.epoll()
    epoll.register(process.stdout.fileno(), select.EPOLLOUT | select.EPOLLIN)
    #
    try:
        while contador < 100:
            contador += 1
            events = epoll.poll(timeout=1)
            for fileno, event in events:
                #
                if event & select.EPOLLIN:
                    if fileno == process.stdout.fileno():
                        read = process.stdout.readline()
                        stdout_lines.append(read)
                        if read == '':
                            print('linea vacía')
                            return 0
                        if read:
                            print(f'linea {contador}: {read.strip()}')
                        if contador == 100:
                            print('contador agotado')
                            process.terminate()
                            return 7
                elif event & select.EPOLLHUP:
                    if fileno == process.stdout.fileno():
                        raise RuntimeError("Program ended.")
    except RuntimeError as err:
        # en realidad no reporta un error
        # sino solo la finalización de la imputación
        # por eso no siempre se imprime
        print(str(err).strip())
    finally:
        epoll.unregister(process.stdout.fileno())
    #
    return process.poll(), stdout_lines


def run_command_1(command):
    """Problema, finaliza antes de imprimir todas las lineas de este comando: run_command_1('ls')"""
    process = subprocess.Popen(shlex.split(command),
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               preexec_fn=os.setsid, encoding='utf8')
    contador = 0
    while contador <= 100:
        contador += 1
        output = process.stdout.readline()
        if output == '':
            print('linea vacía')
            break
        if output:
            print(f'linea {contador}: {output.strip()}')
        if contador == 100:
            print('contador agotado')
            process.terminate()
            break
        if process.poll() is not None:
            print('finalizó la ejecución')
            break
    return process.poll()


if __name__ == '__main__':
    a = run_command('ls')
    b = run_command('pingg google.com')

