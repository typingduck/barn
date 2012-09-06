#!/bin/bash

tee >(                svlogd -tt ./main/ ) \
    >( grep 'INFO\[origin.*x-encoding=\"cat1\".*\]' | svlogd -tt ./cat1/ ) \
    >( grep 'INFO\[origin.*x-encoding=\"cat2\".*\]' | svlogd -tt ./cat2/ ) \
    >( grep 'INFO\[origin.*x-encoding=\"cat3\".*\]' | svlogd -tt ./cat3/ ) \
    > /dev/null


