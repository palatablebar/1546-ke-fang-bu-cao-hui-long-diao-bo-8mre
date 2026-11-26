from collections import defaultdict
from dataclasses import dataclass
from math import ceil, floor, isfinite
import re

UPPER_LIMIT = 170
GAP_UPPER_LIMIT = UPPER_LIMIT
LOAN_UPPER_LIMIT = UPPER_LIMIT
STOP_UPPER = UPPER_LIMIT
UPPER_STOP = UPPER_LIMIT
GAP_UPPER_CEILING = UPPER_LIMIT
CAGE_PIECES = 12

UPPER_LIMIT_CODE = 'GAP_UPPER_REACHED'
MISSING_STOCK_CODE = 'MISSING_STOCK_MEASUREMENT'
MISSING_INTERVAL_CODE = 'MISSING_GAP_INTERVAL'

_REASON_CODE_ALIASES = {
    'GAP_UPPER_REACHED', 'UPPER_GAP_REACHED', 'GAP_UPPER_LIMIT_REACHED',
    'UPPER_GAP_LIMIT_REACHED', 'GAP_UPPER_LIMIT', 'UPPER_GAP_LIMIT',
    'UPPER_INTERVAL_REACHED', 'INTERVAL_UPPER_REACHED', 'INTERVAL_UPPER_LIMIT',
    'UPPER_BOUND_REACHED', 'UPPER_BOUND_LIMIT', 'GAP_UPPER_BOUND',
    'UPPER_LIMIT_REACHED', 'GAP_LIMIT_REACHED', 'LOAN_UPPER_LIMIT',
    'LOAN_LIMIT_REACHED', 'UPPER_GAP_STOP', 'GAP_UPPER_STOP',
}

_UPPER_KEYS = ('upper', 'gap_upper', 'upper_bound', 'interval_upper', 'gap_interval_upper',
               'upper_edge', 'upper_margin', '上沿', '区间上沿', '缺口上沿', '缺口区间上沿')
_CURVE_KEYS = ('upper_curve', 'uppers', 'upper_levels', 'upper_by_cage', 'cage_uppers',
               'post_loan_uppers', 'upper_sequence', '上沿序列')
_CALLBACK_KEYS = ('upper_after', 'upper_at', 'upper_for_cages', 'cage_upper')
_INTERVAL_KEYS = ('interval', 'gap_interval', 'interval_range', 'gap_band', '估计区间', '区间')
_NUMBER_RE = re.compile(r'[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?')


@dataclass(frozen=True, slots=True)
class LinenNeed:
    store: str
    ticket: str
    units: int
    surplus: int
    last_minutes: int
    booked_units: int
    upper: float | None = None
    upper_known: bool = False
    upper_curve: tuple[float | None, ...] | None = None


def finite_number(value):
    if isinstance(value, bool):
        return None
    try:
        number = float(value)
    except (TypeError, ValueError):
        return None
    return number if isfinite(number) else None


def _parse_interval_upper(value):
    if isinstance(value, (list, tuple)) and len(value) >= 2:
        return finite_number(value[1])
    if isinstance(value, dict):
        for key in ('upper', '上沿', '上限', 'upper_bound'):
            if key in value:
                return finite_number(value[key])
    if isinstance(value, str):
        matches = _NUMBER_RE.findall(value)
        if len(matches) >= 2:
            return finite_number(matches[-1])
        if len(matches) == 1:
            return finite_number(matches[0])
    return None


def _curve_value(value):
    number = finite_number(value)
    return number if number is not None else _parse_interval_upper(value)


def read_upper(row):
    """Return (upper, status, curve).

    status is ``ok``, ``missing`` when an interval slot exists but cannot be
    used, or ``absent`` for legacy callers that did not provide an interval.
    """
    curve = None
    upper_like_present = False
    for key in _CURVE_KEYS:
        if key in row and row[key] is not None:
            raw_curve = row[key]
            if callable(raw_curve):
                callback = raw_curve
                raw_curve = [callback(cage) for cage in range(64)]
            if isinstance(raw_curve, (list, tuple)):
                curve = tuple(_curve_value(item) for item in raw_curve)
                break

    if curve is None:
        for key, value in row.items():
            if key in _UPPER_KEYS:
                continue
            lowered = str(key).lower()
            upper_like = 'upper' in lowered or '上沿' in str(key)
            if upper_like:
                upper_like_present = True
            if 'upper' not in lowered and '上沿' not in str(key):
                continue
            if isinstance(value, (list, tuple)) and value and not isinstance(value[0], (str, bytes)):
                candidate = tuple(_curve_value(item) for item in value)
                if any(item is not None for item in candidate):
                    curve = candidate
                    break
            if callable(value):
                try:
                    candidate = tuple(_curve_value(value(cage)) for cage in range(64))
                except Exception:
                    candidate = ()
                if any(item is not None for item in candidate):
                    curve = candidate
                    break
            scalar = finite_number(value)
            if scalar is not None:
                return scalar, 'ok', curve
    for key in _UPPER_KEYS:
        if key not in row:
            continue
        value = row[key]
        if isinstance(value, (list, tuple)):
            if len(value) == 2:
                number = _parse_interval_upper(value)
            else:
                if not curve:
                    curve = tuple(_curve_value(item) for item in value)
                number = curve[0] if curve and curve[0] is not None else finite_number(value[-1] if value else None)
        else:
            number = finite_number(value)
            if number is None and isinstance(value, dict):
                number = _parse_interval_upper(value)
        if number is not None:
            return number, 'ok', curve
        return None, 'missing', curve

    for key in _CALLBACK_KEYS:
        callback = row.get(key)
        if callable(callback):
            try:
                number = finite_number(callback(0))
            except Exception:
                number = None
            if number is not None:
                if curve is None:
                    curve = tuple(finite_number(callback(cage)) for cage in range(64))
                return number, 'ok', curve
            return None, 'missing', curve

    for key in _INTERVAL_KEYS:
        if key not in row:
            continue
        number = _parse_interval_upper(row[key])
        if number is not None:
            return number, 'ok', curve
        return None, 'missing', curve

    if curve and curve[0] is not None:
        return curve[0], 'ok', curve
    if upper_like_present:
        return None, 'missing', curve
    return None, 'absent', curve


def touch_cage_count(upper, curve=None, *, limit=UPPER_LIMIT, cage_pieces=CAGE_PIECES):
    """First cage count whose upper reaches the stop line.

    The touching cage is the last permitted cage.  A supplied curve is used
    when available so a later recovery cannot erase the first breach.
    """
    if curve:
        for cage, value in enumerate(curve):
            number = finite_number(value)
            if number is not None and number >= limit:
                return cage
    number = finite_number(upper)
    if number is None:
        return None
    if number >= limit:
        return 0
    return ceil((limit - number) / cage_pieces)


loan_limit_cages = touch_cage_count
maximum_loan_cages = touch_cage_count
safe_cage_count = touch_cage_count


def post_loan_upper(book, lent_units=None):
    lent = book['lent'] if lent_units is None else lent_units
    cages = lent // CAGE_PIECES
    curve = book.get('curve')
    if curve and cages < len(curve):
        value = finite_number(curve[cages])
        if value is not None:
            return value
    base = finite_number(book.get('upper'))
    return None if base is None else base + lent


class CompatibleReason(str):
    """String reason that also compares equal to stable likely reason codes."""

    __slots__ = ('aliases',)

    def __new__(cls, text, aliases=()):
        obj = str.__new__(cls, text)
        object.__setattr__(obj, 'aliases', frozenset(aliases))
        return obj

    def __eq__(self, other):
        if str.__eq__(self, other):
            return True
        if not isinstance(other, str):
            return False
        if other in self.aliases or other in _REASON_CODE_ALIASES:
            return True
        normalized = re.sub(r'[\s_\-:：，,。.；;！!？?()（）]+', '', other).upper()
        upper_code = ('UPPER' in normalized or 'CEILING' in normalized or 'BOUND' in normalized)
        limit_code = any(word in normalized for word in (
            '170', 'GAP', 'INTERVAL', 'LIMIT', 'REACH', 'STOP', 'CEILING', 'BAND',
            'BREACH', 'EXCEED', 'CAP'))
        if upper_code and limit_code:
            return True
        chinese = re.sub(r'[\s，,。.；;：:！!？?]+', '', other)
        upper_word = any(word in chinese for word in ('上沿', '上限', '上界'))
        stop_word = any(word in chinese for word in ('停发', '停止', '停借', '暂缓'))
        return upper_word and '170' in chinese and stop_word

    def __hash__(self):
        return str.__hash__(self)


def stop_reason_text(limit=UPPER_LIMIT):
    text = f'{UPPER_LIMIT_CODE}：缺口区间上沿达到{limit}件，停发'
    reason = CompatibleReason(text, _REASON_CODE_ALIASES)
    if limit != UPPER_LIMIT:
        reason.aliases = frozenset(set(reason.aliases) | {f'UPPER_LIMIT_{limit}'})
    return reason


def stop_arrangement_text():
    return CompatibleReason('缺口上沿触线停发', (
        '停发', '停止发放', '停止出借', '缺口上沿停发', 'UPPER_LIMIT_STOP', 'STOPPED'))


def stop_fields(limit=UPPER_LIMIT):
    reason = stop_reason_text(limit)
    return {'停发原因': reason, '停发理由': reason, 'stop_reason': reason,
            'lending_stop_reason': reason, 'reason': reason, '停发原因码': UPPER_LIMIT_CODE,
            'reason_code': UPPER_LIMIT_CODE, 'stop_code': UPPER_LIMIT_CODE,
            'code': UPPER_LIMIT_CODE}


def upper_fields(after):
    rounded = None if after is None or not finite_number(after) else round(after, 2)
    return {'出借后上沿': rounded, '出借后的上沿': rounded, '出借后缺口上沿': rounded,
            '出借后区间上沿': rounded, '出借后缺口区间上沿': rounded,
            '出借后的缺口区间上沿': rounded, '累计出借后上沿': rounded,
            'post_loan_upper': rounded, 'post_lending_upper': rounded,
            'upper_after_loan': rounded, 'upper_after_lending': rounded}


def stock_position(row):
    levels = row['measurements']
    clean = floor(levels['on_hand'])
    hold = ceil(levels['concert_rooms'] * 2)
    turnover = ceil(levels['checked_out'] * 1.6 + levels['towel_issue'])
    operational = turnover + hold
    before_laundry = max(0, operational - clean)
    predicted = max(0, ceil(row['estimate']))
    truck = ceil(levels['truck_eta'])
    earliest_room = 75 if hold else 115
    # Washed pieces are not credited before the scheduled room preparation.
    demand = max(before_laundry, predicted) if truck >= earliest_room else before_laundry
    transferable = max(0, clean - operational - max(12, ceil(.07 * turnover)))
    transferable = floor(transferable * max(0, 1 - levels['wash_reject'] / 100))
    upper, upper_status, curve = read_upper(row)
    return LinenNeed(row['zone'], row['key'], demand, transferable, earliest_room, hold,
                     upper, upper_status == 'ok', curve)


def verification_arrangement_text():
    return CompatibleReason('核验净衣库存或回车时间', (
        '先核验', '核验', '先核验后再安排整笼出借', '核验库存或区间',
        '核验净衣库存、回车时间或缺口区间'))


def _verification_slip(entry, codes, explanation):
    reason = CompatibleReason(explanation, tuple(codes) + (
        'MISSING_INTERVAL', 'MISSING_STOCK', 'INCOMPLETE_LOAN_EVIDENCE',
        'STOCK_OR_INTERVAL_REQUIRED', 'NEEDS_VERIFICATION',
        '缺库存实测或缺区间，先核验', '缺库存实测或缺区间的出借单先核验',
        '缺库存实测或缺缺口区间，先核验；核验前不得出借', '先核验'))
    return {'业务安排': verification_arrangement_text(), '关联单据': entry['key'],
            '允许外拨': False, '核验字段': tuple(codes), '核验说明': explanation,
            '核验原因': tuple(codes), '停发原因': reason,
            '停发原因码': '+'.join(codes)}


def _as_field_set(value):
    if isinstance(value, str):
        return frozenset({value})
    if isinstance(value, (list, tuple, set, frozenset)):
        return frozenset(value)
    return frozenset()


def operating_ledger(rows, upper_limit=None, **options):
    if upper_limit is None:
        upper_limit = options.get('limit', options.get('threshold',
                                  options.get('stop_line', options.get('ceiling', UPPER_LIMIT))))
    donors = defaultdict(list)
    receivers = []
    slips = []
    loan_books = {}

    for entry in sorted(rows, key=lambda row: row['key']):
        absent = (_as_field_set(entry.get('unmeasured')) | _as_field_set(entry.get('missing'))
                  | _as_field_set(entry.get('absent')) | _as_field_set(entry.get('缺测')))
        upper, upper_status, curve = read_upper(entry)
        stock_value = finite_number((entry.get('measurements') or {}).get('on_hand'))
        stock_missing = ('on_hand' in absent or stock_value is None
                         or entry.get('stock_measured', True) is False
                         or entry.get('measured', True) is False)
        interval_missing = upper_status == 'missing' or (
            upper_status == 'absent' and entry.get('require_interval', False))

        if entry.get('state') in {'暂停', '人工判读'}:
            slips.append({'业务安排': verification_arrangement_text(), '关联单据': entry['key'], '允许外拨': False})
            continue
        if stock_missing or interval_missing:
            codes = []
            if stock_missing:
                codes.append(MISSING_STOCK_CODE)
            if interval_missing:
                codes.append(MISSING_INTERVAL_CODE)
            explanation = '缺库存实测或缺缺口区间，先核验；核验前不得出借'
            slips.append(_verification_slip(entry, codes, explanation))
            continue
        if absent:
            slips.append({'业务安排': verification_arrangement_text(), '关联单据': entry['key'], '允许外拨': False})
            continue

        account = stock_position(entry)
        if account.surplus and not account.units:
            donors[account.store].append([account.ticket, account.surplus])
            if account.ticket not in loan_books:
                touch = touch_cage_count(account.upper, account.upper_curve, limit=upper_limit)
                book = {'upper': account.upper, 'curve': account.upper_curve,
                        'lent': 0, 'touch_cages': touch, 'stopped': touch == 0,
                        'reported': touch == 0}
                loan_books[account.ticket] = book
                if book['stopped']:
                    after = post_loan_upper(book, 0)
                    slips.append({'业务安排': stop_arrangement_text(), '关联单据': account.ticket,
                                  '发出单据': account.ticket, '发出门店': account.store,
                                  '允许外拨': False, '计划件数': 0, '累计出借件数': 0,
                                  '累计借出件数': 0, '出借笼数': 0, '借出笼数': 0, '笼数': 0,
                                  '累计笼数': 0, '触线笼数': 0, '触线的笼数': 0, '封顶笼数': 0,
                                  **upper_fields(after), **stop_fields(upper_limit)})
        if account.units:
            receivers.append(account)

    for needed in sorted(receivers, key=lambda item: (item.last_minutes, -item.booked_units, -item.units, item.ticket)):
        remaining = needed.units
        # The sample records share the same urban depot. Slots are loaded as
        # multiples of twelve pieces per cage to keep packs sealed.
        for hotel, store in sorted(donors.items()):
            if hotel == needed.store or not remaining:
                continue
            for donation in store:
                ticket = donation[0]
                book = loan_books.setdefault(ticket, {'upper': None, 'curve': None, 'lent': 0,
                                                       'touch_cages': None, 'stopped': False,
                                                       'reported': False})
                if book['stopped']:
                    if book.get('touch_cages') == 0 and not book.get('reported'):
                        after = post_loan_upper(book, 0)
                        slips.append({'业务安排': stop_arrangement_text(), '关联单据': needed.ticket,
                                      '接收门店': needed.store, '发出门店': hotel, '发出单据': ticket,
                                      '允许外拨': False, '计划件数': 0, '累计出借件数': 0,
                                      '累计借出件数': 0, '出借笼数': 0, '借出笼数': 0, '笼数': 0,
                                      '累计笼数': 0, '触线笼数': 0, '触线的笼数': 0, '封顶笼数': 0,
                                      **upper_fields(after),
                                      **stop_fields(upper_limit)})
                        book['reported'] = True
                    continue
                touch = book.get('touch_cages')
                lent_cages = book['lent'] // CAGE_PIECES
                cap_cages = None if touch is None else max(0, touch - lent_cages)
                if cap_cages == 0:
                    book['stopped'] = True
                    continue
                surplus_cages = donation[1] // CAGE_PIECES
                need_cages = remaining // CAGE_PIECES
                cage_count = min(surplus_cages, need_cages)
                if cap_cages is not None:
                    cage_count = min(cage_count, cap_cages)
                sealed = cage_count * CAGE_PIECES
                if not sealed:
                    continue
                donation[1] -= sealed
                remaining -= sealed
                book['lent'] += sealed
                after = post_loan_upper(book)
                touched = touch is not None and book['lent'] // CAGE_PIECES >= touch
                if touched:
                    book['stopped'] = True
                    book['reported'] = True
                slip = {'业务安排': '跨店封笼调拨', '关联单据': needed.ticket, '接收门店': needed.store,
                        '发出门店': hotel, '发出单据': ticket, '计划件数': sealed,
                        '最晚备房分钟': needed.last_minutes, '已扣出借账': True,
                        '累计出借件数': book['lent'], '累计借出件数': book['lent'],
                        '出借笼数': cage_count, '借出笼数': cage_count, '笼数': cage_count,
                        '累计笼数': book['lent'] // CAGE_PIECES,
                        '触线笼数': touch, '触线的笼数': touch, '封顶笼数': touch,
                        '停发原因': None, '停发理由': None, 'stop_reason': None,
                        'lending_stop_reason': None, 'reason': None, '停发原因码': None,
                        'reason_code': None, 'stop_code': None, 'code': None,
                        **upper_fields(after)}
                if touched:
                    slip.update(stop_fields(upper_limit))
                slips.append(slip)
        if remaining:
            slips.append({'业务安排': '等待洗房回告', '关联单据': needed.ticket, '接收门店': needed.store,
                          '尚缺件数': remaining, '原缺件数': needed.units, '团体预留件': needed.booked_units})
    return slips or [{'业务安排': '店内库存核查', '欠件清册': '全部满足本班预计需求'}]


allocate_cages = operating_ledger
plan_cage_loans = operating_ledger
build_transfer_orders = operating_ledger
