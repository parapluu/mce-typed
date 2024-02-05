import { driver_loop, setup_environment } from './mce';

const the_global_environment = setup_environment();

driver_loop(the_global_environment, "--- session start ---");