declare module 'sicp' {
    export type Pair<H, T> = [H, T];
    export type List<T> = [T, List<T>] | null;

    export function is_boolean(value: any): boolean;
    export function set_head<H, T>(pair: Pair<H, T>, elem: H): Pair<H, T>;
    export function is_pair(value: any): boolean;
    export function list_ref<T>(list: List<T>, index: number): T;
    export function length<T>(list: List<T>): number;
    export function apply_in_underlying_javascript(f: any, args: any): any;
    export function pair<H,T>(head: H, tail: T): Pair<H, T>;
    export function stringify(value: any): string;
    export function is_null(value: any): boolean;
    export function error(...args: [any]): any;
    export function math_abs(value: number): number;
    export const math_PI: number;
    export const math_E: number;
    export function display(...x: any[]): any;
    export function map<H, T>(fun: (from: H) => T, list: List<H>): List<T>;
    export function accumulate<A, T>(x: (a: T, b: A) => A, initial: A, list: List<T>): A;
    export function parse(x: any): any;
    export function append<T>(a: List<T>, b: List<T>): List<T>;
    export function head<H, T>(pair: Pair<H, T>): H;
    export function tail<H, T>(pair: Pair<H, T>): T;

    export function list<T>(...elems: [T]): List<T>;
    // Because the above requires at least one argument
    export function list<T>(): List<T>; 
}

