@echo off
chcp 65001 >nul

:: =============================================
:: Auto-commit script for SWIFT-AutoLISP
:: Usage: double-click or run from command line
:: =============================================

echo [Auto-Commit] Starting...

git status --porcelain
if %errorlevel% neq 0 (
    echo Error: Not a git repository or git not found.
    pause
    exit /b 1
)

:: Check if there are changes
git diff --quiet --exit-code
if %errorlevel% equ 0 (
    git diff --cached --quiet --exit-code
    if %errorlevel% equ 0 (
        echo No changes to commit.
        pause
        exit /b 0
    )
)

echo.
echo Changes detected. Committing...

git add -A

:: Generate commit message
for /f "tokens=1-3 delims=. " %%a in ('date /t') do set mydate=%%c-%%b-%%a
for /f "tokens=1-2 delims=: " %%a in ('time /t') do set mytime=%%a:%%b

git commit -m "auto: update %mydate% %mytime%"

if %errorlevel% neq 0 (
    echo Commit failed.
    pause
    exit /b 1
)

echo Pushing to GitHub...
git push

if %errorlevel% equ 0 (
    echo [SUCCESS] Changes committed and pushed.
) else (
    echo Push failed. Check your connection or credentials.
)

echo.
echo Done.
pause
