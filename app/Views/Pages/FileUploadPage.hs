{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.FileUploadPage (fileUploadPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Layouts as Layouts

fileUploadPage :: Html.Html
fileUploadPage = Layouts.modalLayout "Song Upload"
    [hsx|
        <form action="/music" method="post" enctype="multipart/form-data" class="flex flex-col gap-4">
            <input type="text" name="song_name" placeholder="Song Title" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="text" name="author_name" placeholder="Artist Name" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="text" name="album_name" placeholder="Album Name" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="date" name="release_date" placeholder="Release Date" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="text" name="length" placeholder="Length" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="file" name="file" class="block w-full text-sm text-gray-500
                file:mr-4 file:py-2 file:px-4
                file:rounded-full file:border-0
                file:text-sm file:font-semibold
                file:bg-blue-500 file:text-white
                hover:file:bg-blue-600"/>
            <button type="submit" class="py-2 px-4 bg-blue-500 hover:bg-blue-700 rounded-full text-white font-bold">Upload</button>
        </form>
        <script>
            const date = new Date()
            const now_utc = Date.UTC(date.getUTCFullYear(), date.getUTCMonth(),
                date.getUTCDate(), date.getUTCHours(),
                date.getUTCMinutes(), date.getUTCSeconds());
            const newdate = new Date(now_utc);
            console.log(newdate.toISOString());
        </script>
|]
