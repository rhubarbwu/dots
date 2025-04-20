import os
import platform
import shutil
from sys import argv
import sqlite3


def get_history_path():
    if platform.system() == "Darwin":
        history_path = "~/Library/Application Support"
    elif platform.system() == "Linux":
        history_path = "~/.config"
    else:
        raise ValueError(f"Unsupported OS: {platform.system()}")
    history_path += "/BraveSoftware/Brave-Browser/Default/History"

    expanded_path = os.path.expanduser(history_path)
    if not os.path.exists(expanded_path):
        raise FileNotFoundError(f"History file not found at: {expanded_path}")

    return history_path


def main(patterns):
    history_path = get_history_path()

    history_db = os.path.expanduser(history_path)
    backup_db = history_db + ".backup"
    shutil.copy2(history_db, backup_db)
    print(f"Backup created: {backup_db}")

    try:  # try connecting to database, see if browser is off
        test_conn = sqlite3.connect(history_db, timeout=1)
        test_conn.close()
    except sqlite3.OperationalError as e:
        if "database is locked" in str(e):
            print(
                "Error: Database is locked. Please close Brave browser completely and try again."
            )
            return

    # attempt to delete matches
    success = False
    try:
        conn = sqlite3.connect(history_db, timeout=10)
        cursor = conn.cursor()

        for url_pattern in patterns:
            # count and fetch matches
            cursor.execute(f"SELECT COUNT(*) FROM urls WHERE url LIKE '{url_pattern}';")
            match_count = cursor.fetchone()[0]
            if match_count == 0:
                print(f"No matches found for '{url_pattern}'")
                continue
            cursor.execute(f"SELECT url FROM urls WHERE url LIKE '{url_pattern}';")
            matches = cursor.fetchall()

            # display matches to user, truncated to fit standard 80-width terminal
            print(f"Found {match_count} matches for '{url_pattern}':")
            for match in matches:
                url = match[0]
                if len(url) > 76:  # 76 + 2 spaces = 78
                    url = url[:73] + "..."
                print(f"  {url}")

            # prompt user to confirm deletion
            confirm = input(f"Delete these {match_count} matches? (Y/n): ").upper()
            if confirm.startswith("Y"):
                cursor.execute(f"DELETE FROM urls WHERE url LIKE '{url_pattern}';")
                print(f"Deleted {match_count} matches for: {url_pattern}")
            else:
                print(f"Skipped deleting {match_count} matches for: {url_pattern}")

        conn.commit()
        cursor.execute("VACUUM;")
        conn.commit()

        print("History entries deleted successfully.")
        success = True

    except Exception as e:
        print(f"Error: {e}")

    finally:
        if "cursor" in locals() and cursor:
            cursor.close()
        if "conn" in locals() and conn:
            conn.close()

        if success and os.path.exists(backup_db):
            os.remove(backup_db)
            print(f"Backup deleted: {backup_db}")


if __name__ == "__main__":
    patterns = [f"%{pattern}%" for pattern in argv[1:]]
    main(patterns)
