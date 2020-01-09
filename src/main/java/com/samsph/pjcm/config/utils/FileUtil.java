package com.samsph.pjcm.config.utils;

import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import org.springframework.web.multipart.MultipartFile;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;

public class FileUtil {
    /**
     * 生成随机文件名：五位随机数+当前年月日时分秒
     *
     * @return
     */
    public static String getRandomFileName() {
        SimpleDateFormat simpleDateFormat;
        simpleDateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
        Date date = new Date();
        String str = simpleDateFormat.format(date);
        Random random = new Random();
        int num = (int) (random.nextDouble() * (99999 - 10000 + 1)) + 10000;// 获取5位随机数
        return num + str;// 当前时间
    }

    /**
     * 上传文件到服务器
     * @param parentPath 文件存储的目录
     * @param file  文件
     * @return
     */
    public static String FileUpload(String parentPath, MultipartFile file){
        if (file.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "文件为空,请选择一个文件上传!");
        }
        String suffixName = file.getOriginalFilename().substring(file.getOriginalFilename().lastIndexOf("."));
        String fileName = FileUtil.getRandomFileName() + suffixName;
        String pathname = parentPath + fileName;
        File dir = new File(parentPath);
        if(!dir.exists()){
            dir.mkdirs();
        }
        File file1 = new File(pathname);
        if(!file1.exists()){
            try{
                file1.createNewFile();
                FileOutputStream fileOutputStream = new FileOutputStream(file1);
                fileOutputStream.write(file.getBytes());
                fileOutputStream.close();
            }catch (IOException e){
                e.printStackTrace();
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"文件上传失败!!!");
            }
        }
        return pathname;
    }

    /**
     * 覆盖已有文件
     * @param path  旧文件原来的路径
     * @param file  新文件
     * @return
     */
    public static String FileCover(String path, MultipartFile file) {
        String head = path.substring(0, path.lastIndexOf("."));
        String fileName = file.getOriginalFilename();
        //后缀名
        String suffixName = fileName.substring(fileName.lastIndexOf("."));
        File dest = new File(path);
        //源文件不存在了
        if(!dest.exists()){
            try{
                dest.createNewFile();
                FileOutputStream fileOutputStream = new FileOutputStream(dest);
                fileOutputStream.write(file.getBytes());
                fileOutputStream.close();
                return head + suffixName;
            }catch (IOException e){
                e.printStackTrace();
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"文件上传失败!!!");
            }
        }
        //源文件存在，比较后缀是否一样
        if (!suffixName.equals(path.substring(path.lastIndexOf(".")))) {
            if(!delFile(dest)){
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"删除文件失败！！！");
            }
            dest = new File(head + suffixName);
            if(!dest.exists()) {
                try {
                    dest.createNewFile();
                    FileOutputStream fileOutputStream = new FileOutputStream(dest);
                    fileOutputStream.write(file.getBytes());
                    fileOutputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                    throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"文件上传失败!!!");
                }
            }
        }else {
            try{
                FileOutputStream fileOutputStream = new FileOutputStream(dest);
                fileOutputStream.write(file.getBytes());
                fileOutputStream.close();
            }catch (IOException e){
                e.printStackTrace();
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"文件上传失败!!!");
            }
        }
        return head + suffixName;
    }

    /**
     * 删除文件
     * @param file 打开后的文件
     * @return
     */
    private static boolean delFile(File file) {
        if (!file.exists()) {
            return false;
        }
        if (file.isFile()) {
            return file.delete();
        } else {
            File[] files = file.listFiles();
            for (File f : files) {
                delFile(f);
            }
            return file.delete();
        }
    }

    /**
     * 删除文件
     * @param filePath 文件路径
     */
    public static void deleteFile(String filePath){
        File file = new File(filePath);
        if(!delFile(file)){
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"删除文件失败！");
        }
    }
}