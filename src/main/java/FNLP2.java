import org.fnlp.nlp.cn.CNFactory;

import java.util.HashMap;

/**
 * Author: Watson Lin
 * Date: 15/4/3
 */
public class FNLP2 {

    public static void main(String[] args) throws Exception {
        // 创建中文处理工厂对象，并使用“models”目录下的模型文件初始化
        CNFactory factory = CNFactory.getInstance("models");

        // 使用标注器对包含实体名的句子进行标注，得到结果
        HashMap<String, String> result = CNFactory.ner(
                "詹姆斯·默多克和丽贝卡·布鲁克斯 鲁珀特·默多克旗下的美国小报《纽约邮报》的职员被公司律师告知，保存任何也许与电话窃听及贿赂有关的文件。");

        // 显示标注结果
        System.out.println(result);

    }
}
