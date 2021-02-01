import socket
from dataclasses import dataclass
from enum import Enum
from typing import List, Union

import datetime
import time

HOST = "localhost"
PORT = 3000


ENDIAN = "big"


class NoReceivedDataError(Exception):
    pass


class CheckSumError(Exception):
    pass


class IncommingCheckSumError(Exception):
    pass


class ResourceDoesntExistError(Exception):
    pass


class ChecksumError(Exception):
    pass


class SizeMismatchError(Exception):
    pass


class TooBigError(Exception):
    pass


class ServerMethod(Enum):
    PING = b"\1"
    GET = b"\2"
    LIST = b"\3"
    PATCH = b"\4"


@dataclass
class IndexableData:
    index: bytes
    data: bytes


def prepare_message(method: ServerMethod, content: bytes) -> bytes:
    method = method.value

    # size
    size_size = 8
    method_size = 1
    content_size = len(content)
    checksum_size = 1

    size = size_size + method_size + content_size + checksum_size
    size_bytes = size.to_bytes(8, ENDIAN)

    data_without_checksum = size_bytes + method + content

    checksum = calculate_checksum(data_without_checksum)
    return data_without_checksum + checksum.to_bytes(1, ENDIAN)


def ping_message() -> str:
    return prepare_message(ServerMethod.PING, b"")


def list_message(resource_name: bytes) -> bytes:
    return prepare_message(ServerMethod.LIST, resource_name)


def get_message(resource_name: bytes, resource_index: bytes) -> bytes:
    resource_name_size_bytes = len(resource_name).to_bytes(4, ENDIAN)

    content = resource_name_size_bytes + resource_name + resource_index
    return prepare_message(ServerMethod.GET, content)


def patch_message(resource_name: bytes, list_data: list[IndexableData]) -> bytes:
    def index_data_to_bytes(data: IndexableData):
        index_size_bytes = len(data.index).to_bytes(4, ENDIAN)
        data_size_bytes = len(data.data).to_bytes(4, ENDIAN)
        return index_size_bytes + data.index + data_size_bytes + data.data

    items = b"".join([index_data_to_bytes(i) for i in list_data])
    resource_name_size_bytes = len(resource_name).to_bytes(4, ENDIAN)
    content = resource_name_size_bytes + resource_name + items

    return prepare_message(ServerMethod.PATCH, content)


def check_response_code(status_code_byte: int) -> None:
    if status_code_byte == 1:  # OK
        return

    if status_code_byte == 2:  # Resource doesn't exist
        raise ResourceDoesntExistError


def parse_resource_list(content: bytes) -> list[bytes]:
    content_length = len(content)
    status_code = content[0]
    check_response_code(status_code)

    # TODO IO stream instead would be better

    data = []
    pointer = 1
    while pointer < content_length:
        segment_size = int.from_bytes(content[pointer : pointer + 4], ENDIAN)
        pointer += 4
        segment_content = content[pointer : pointer + segment_size]
        pointer += segment_size
        data.append(segment_content)

    return data


def calculate_checksum(data: bytes) -> int:
    lrc = 0
    for b in data:
        lrc = (lrc + b) & 0xFF
    return ((lrc ^ 0xFF) + 1) & 0xFF



class Connection:
    def __init__(self, host: str, port: str) -> None:
        self._host = host
        self._port = port
        self._connection = None

    def connect(self) -> None:
        self._connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._connection.connect((HOST, PORT))

    def close(self) -> None:
        self._connection.close()

    def _receive_all(self, input_size: int) -> bytes:
        chunk_size = 30000
        data = bytearray()

        while len(data) != input_size:
            response = self._connection.recv(chunk_size)
            data.extend(response)

        return data

    def execute(self, command: bytes) -> None:
        if self._connection is None:
            self.connect()

        self._connection.sendall(command)
        # print(f"Sent:\n    {command}")

        data_size = self._connection.recv(8)
        size = int.from_bytes(data_size, ENDIAN)
        data = self._receive_all(size - 8)
        content = data[:-1]
        their_checksum = data[-1]
        # print(f"Received (conten-size = {size}):\n    {data}")

        our_checksum = calculate_checksum(data_size + data[:-1])
        if our_checksum != their_checksum:
            print(f"our checksum = {int(our_checksum)}, their checksum = {int(their_checksum)}")
            raise IncommingCheckSumError

        if content == b"CHECKSUMERROR":
            raise CheckSumError
        elif content == b"NOTHING_RECEIVED":
            raise NoReceivedDataError
        elif content == b"SIZE_MISMATCH":
            raise SizeMismatchError
        elif content == b"TOO_BIG":
            raise TooBigError

        return content  # without the checksum byte


class Executor:
    def __init__(self, connection: Connection) -> None:
        self._connection = connection

    def ping(self) -> bytes:
        message = ping_message()
        return self._connection.execute(message)

    def list(self, resource_name: bytes) -> List[bytes]:
        message = list_message(resource_name)
        response = self._connection.execute(message)
        return parse_resource_list(response)

    def get(self, resource_name: bytes, resource_index: bytes) -> List[bytes]:
        message = get_message(resource_name, resource_index)
        response = self._connection.execute(message)
        return parse_resource_list(response)

    def patch(self, resource_name: bytes, data: List[IndexableData]) -> None:
        message = patch_message(resource_name, data)
        self._connection.execute(message)


if __name__ == "__main__":
    connection = Connection(HOST, PORT)
    client = Executor(connection)

    existing_resource_name = b"test-resource"
    test_resource_name = b"another-resource"
    another_test_resource_name = b"another-resource"
    test_data = [
        IndexableData(data=b"test-data-1", index=b"1"),
        IndexableData(data=b"another super awesome data", index=b"2"),
    ] + [
        IndexableData(
            data=b"super ultra mega penis cock hyper long message",
            index=f"omfg-this-so-ultra-long-{i}".encode("utf-8"),
        )
        for i in range(30000)
    ]

    client.ping()

    t1 = datetime.datetime.now()
    client.patch(another_test_resource_name, test_data)
    t2 = datetime.datetime.now()

    print("PATCH", (t2 - t1).microseconds / 1000)

    for _ in range(10):
        t1 = datetime.datetime.now()
        client.list(another_test_resource_name)
        t2 = datetime.datetime.now()
        print("LIST", (t2 - t1).microseconds / 1000)

    for i in range(10):
        t1 = datetime.datetime.now()
        res = client.get(another_test_resource_name, f"omfg-this-so-ultra-long-{i}".encode("utf-8"))
        print(res)
        t2 = datetime.datetime.now()
        print("GET", (t2 - t1).microseconds / 1000)

    connection.close()
