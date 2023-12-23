{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.FileUploadPage (fileUploadPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Layouts as Layouts

fileUploadPage :: Html.Html
fileUploadPage = Layouts.modalLayout "Song Upload"
    [hsx|
        <form action="/upload-song" method="post" enctype="multipart/form-data" class="flex flex-col gap-4">
            <input type="text" name="songTitle" placeholder="Song Title" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="text" name="artistName" placeholder="Artist Name" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="text" name="albumName" placeholder="Album Name" class="px-4 py-2 rounded bg-gray-700 text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
            <input type="file" name="file" class="block w-full text-sm text-gray-500
                file:mr-4 file:py-2 file:px-4
                file:rounded-full file:border-0
                file:text-sm file:font-semibold
                file:bg-blue-500 file:text-white
                hover:file:bg-blue-600"/>
            <button type="submit" class="py-2 px-4 bg-blue-500 hover:bg-blue-700 rounded-full text-white font-bold">Upload</button>
        </form>
|]
